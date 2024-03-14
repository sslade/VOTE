
;;;;------------------------------------------------------------------------
;;;;   verbs
;;;;------------------------------------------------------------------------


;;;;------------------------------------------------------------------------
;;         verb procedures and globals defined below
;;  
;;  (gen-verb subject verb passive? tense)
;;  (conjugate-verb person-number-tense verb)
;;  (gen-assoc features alist)
;;  (gen-verb-ending verb ending)
;;  (irregular-verb-alists verb)
;;  (conjugation verb passive)
;;  *irregular-tense-alist*
;;  *regular-verb-alist*
;;  *regular-e-verb-alist*
;;  *regular-y-verb-alist*
;;    
;;;;------------------------------------------------------------------------


;;  generate the properly inflected verb from the following parameters: 
;;  
;;  subject   a token from which features of gender, number, and person may be extracted
;;  verb      an infinitive given as a string
;;  passive?  flag to indicate passive voice
;;  tense     one of: present     "I am"
;;                    past        "I was"
;;                    perf        "I have been"
;;                    plup        "I had been"
;;                    ppart       "been"
;;                    future      "I will be"
;;                    futperf     "I will have been"
;;                    infinitive  "to be"

(defun gen-verb (subject verb passive? tense &rest gerund)  
  (let ((number 'sing)
	(person 'third)
;;(let ((number (gen-get-feature subject 'number))
;;        (person (gen-get-feature subject 'person))
        (tense  (if (null tense)
                    'present
                    tense)))
    (cond ((car gerund)
           (concatenate 'string 
            (gen-verb subject "be" nil tense)
            " "
            (conjugate-verb (list person number 'gerund) verb)))
          (passive?
           (concatenate 'string 
            (gen-verb subject "be" nil tense)
            " "
            (conjugate-verb (list person number 'ppart) verb)))
          (else
           (case tense
             ((infinitive) (concatenate 'string "to " verb))
             ((future)     (concatenate 'string 
                            (gen-verb subject "will" nil 'present)
                            " "
                            verb))
             ((futperf)    (concatenate 'string 
                            (gen-verb subject "will" nil 'present)
                            " have "
                            (gen-verb subject verb nil 'ppart)))
             ((perf)       (concatenate 'string 
                            (gen-verb subject "have" nil 'present)
                            " "
                            (gen-verb subject verb nil 'ppart)))
             ((plup)       (concatenate 'string 
                            (gen-verb subject "have" nil 'past)
                            " "
                            (gen-verb subject verb nil 'ppart)))
             ((ppart)       (conjugate-verb (list person number 'ppart) verb))
             (otherwise
              (conjugate-verb (list person number tense) verb)))))))



;; inflects / conjugates regular and irregular verbs
;;  
;;  > (conjugate-verb '(first sing present) "love")
;;  "love"
;;  > (conjugate-verb '(third sing present) "love")
;;  "loves"
;;  > (conjugate-verb '(third sing present) "be")
;;  "is"
;;  > (conjugate-verb '(second sing present) "be")
;;  "are"
;;  > (conjugate-verb '(first sing present) "be")
;;  "am"
;;  > (conjugate-verb '(first plur past) "be")
;;  "were"
;;  > (conjugate-verb '(third sing past) "carry")
;;  "carried"
;;  > (conjugate-verb '(third sing present) "carry")
;;  "carries"
;;  > (conjugate-verb '(third sing present) "arbitrate")
;;  "arbitrates"
;;  > (conjugate-verb '(third sing past) "move")
;;  "moved"
;;  
(defun conjugate-verb (person-number-tense verb)  
  (let* ((irr-alist (irregular-verb-alists verb))
         (tense (caddr person-number-tense))
         (y-ending (member (lastchar verb) '(#\y #\Y)))
         (e-ending (member (lastchar verb) '(#\e #\E)))
         (s-ending (member (lastchar verb) '(#\s #\S #\x #\X #\z #\Z)))
         (new-verb (coerce (reverse (cdr (reverse (coerce verb 'list)))) 'string))
         (alist (cond (y-ending *regular-y-verb-alist*)
                      (e-ending *regular-e-verb-alist*)
                      (s-ending *regular-s-verb-alist*)
                      (else *regular-verb-alist*))))
    (cond ((cdr (assoc (list verb tense) *irregular-tense-alist* :test #'equalp)))
          ((and irr-alist
                (gen-assoc person-number-tense irr-alist)))
          (else
           (let ((ending (gen-assoc person-number-tense alist)))
             (if ending
               (gen-verb-ending (if (or (and (eq tense 'gerund)
                                             e-ending)
                                        (and (not (eq tense 'gerund))
                                             y-ending))
                                  new-verb
                                  verb)
                                ending)
               verb))))))


(defun gen-assoc (features alist)  
  (cdr 
   (cond ((assoc features alist :test #'equalp))
         ((assoc (cons '* (cdr features)) alist :test #'equalp))
         ((assoc (append '(* *) (cddr features)) alist :test #'equalp))
         (else nil))))

(defun gen-verb-ending (verb ending)  
  (cond (ending (concatenate 'string verb ending))
        (else verb)))
                         

(defun irregular-verb-alists (verb)  
  (case (->symbol verb)
    ((be) '(((first sing present) .  "am")
            ((third sing present) .  "is")
            (( *     *   present) .  "are")
            (( *     *   ppart)   .  "been")
            (( *     *   gerund)  .  "being")
            (( *    sing past)    .  "was")
            (( *    plur past)    .  "were")))
    ((can) '(((third sing present) .  "can")
            (( *     *   ppart)   .  "been able to")
            (( *     *   past)    .  "could")))
    ((do) '(((third sing present) .  "does")
            (( *     *   ppart)   .  "done")
            (( *     *   past)    .  "did")))
    ((go) '(((third sing present) .  "goes")
            (( *     *   ppart)   .  "gone")
            (( *     *   past)    .  "went")))
    ((have) '(((third sing present) .  "has")
              (( *     *   present) .  "have")
              (( *     *   ppart)   .  "had")
              (( *     *   past)    .  "had")))
    ((will) '(((third sing present) .  "will")
              ((first sing present) .  "shall")
              ((first plur present) .  "shall")))
    (otherwise nil)))

;;  >  (conjugation "be" nil)
;;  
;;  Tense:    Person:   Singular:                     Plural:
;;  
;;    PRESENT
;;              FIRST     I am                          we are
;;              SECOND    you are                       you are
;;              THIRD     he is                         they are
;;    PAST
;;              FIRST     I was                         we were
;;              SECOND    you was                       you were
;;              THIRD     he was                        they were
;;    PERF
;;              FIRST     I have been                   we have been
;;              SECOND    you have been                 you have been
;;              THIRD     he has been                   they have been
;;    PLUP
;;              FIRST     I had been                    we had been
;;              SECOND    you had been                  you had been
;;              THIRD     he had been                   they had been
;;    FUTURE
;;              FIRST     I shall be                    we shall be
;;              SECOND    you will be                   you will be
;;              THIRD     he will be                    they will be
;;    FUTPERF
;;              FIRST     I shall have been             we shall have been
;;              SECOND    you will have been            you will have been
;;              THIRD     he will have been             they will have been
;;

(defun conjugation (verb passive &rest gerund)  
  (format t "~%Tense: ~8TPerson:~16TSingular: ~44TPlural:~%")
  (mapc 
   #'(lambda (tense)
     (format t "~%~a" tense)
     (mapc 
      #'(lambda (person)
        (format t "~%~8T~A~16T~A ~A ~44T~A ~A"
                person
                (gen-pronoun2 (list 'male 'sing person 'subj 't))
                (gen-verb (verb-test-person person 'sing 'male) verb passive tense (car gerund))
                (gen-pronoun2 (list 'male 'plur person 'subj 't))
                (gen-verb (verb-test-person person 'plur 'male) verb passive tense (car gerund))))
      '(first second third)))
   '(present past perf plup future futperf))
  (values))

;;  The following code results in a list like this:
;;  
;;    '((("bring" past) . "brought")
;;      (("feel" past) . "felt")
;;      (("feel" ppart) . "felt")
;;      ...
;;      
(setf *irregular-tense-alist*   
      (mappend
       #'(lambda (triple)
           (let ((verb (first triple))
                 (past (second triple))
                 (ppart (third triple)))
             (list
              (cons (list verb 'past) past)
              (cons (list verb 'ppart) ppart))))
       '(("become" "became" "become")
         ("bring" "brought" "brought")
         ("buy" "bought" "bought")
         ("come" "came" "come")
         ("drink" "drank" "drunk")
         ("eat" "ate" "eaten")
         ("feel" "felt" "felt")
         ("fight" "fought" "fought")
         ("get" "got" "gotten")
         ("give" "gave" "given")
         ("grab" "grabbed" "grabbed")
         ("hear" "heard" "heard")
         ("hit" "hit" "hit")
         ("know" "knew" "known")
         ("make" "made" "made")
         ("must" "must" "must")
         ("read" "read" "read")
         ("ring" "rang" "rung")
         ("see" "saw" "seen")
         ("sell" "sold" "sold")
         ("should" "should" "should")
         ("sing" "sang" "sung")
         ("take" "took" "taken")
         ("tell" "told" "told")
         ("think" "thought" "thought")
         ("throw" "threw" "thrown"))))


(setf *regular-verb-alist*      
  '(((third sing present) . "s")
    (( *     *   ppart)   . "ed")
    (( *     *   gerund)  . "ing")
    (( *     *   past)    . "ed")))
     
;; endings for regular verbs that end in "e"
;; believe --> believed

(setf *regular-e-verb-alist*      '(((third sing present) . "s")
       (( *     *   gerund)  . "ing")
       (( *     *   ppart)   . "d")
       (( *     *   past)    . "d")))

;; endings for regular verbs that end in "s"
;; stress --> stresses

(setf *regular-s-verb-alist*      '(((third sing present) . "es")
       (( *     *   gerund)  . "ing")
       (( *     *   ppart)   . "ed")
       (( *     *   past)    . "ed")))

;; endings for regular verbs that end in "y"
;; carry --> carried

(setf *regular-y-verb-alist*      '(((third sing present) . "ies")
       (( *     *   gerund)  . "ing")
       (( *     *   ppart)   . "ied")
       (( *     *   past)    . "ied")))
     
