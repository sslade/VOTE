
;; (english self)
;; operation for producing english phrase for a structure
;;-------------------------------------------------------
(defmethod english (self)
  (cond ((and (record-p self)
              (slot-value self 'name)))
        (else nil)))


;;;;------------------------------------------------------------------------
;;;;    stances
;;;;------------------------------------------------------------------------

;;  State a generic stance


(defun eng-stance (subject stance)  
  (let ((st (cond ((flip-stance stance))
                  (else stance))))
    (concatenate 'string (gen-np subject 'subj)
                   (eng-stance2 subject st)
                   (eng-stance-issue st)
                   ".")))


(defun flip-stance (stance)  
  (let ((opp (issue-opposite (reveal-issue stance))))
    (cond ((and (eq (stance-side stance) 'con)
                opp)
           (let ((st1 (copy stance)))
             (setf (stance-side st1) 'pro)
             (setf (stance-issue st1) opp)
             (setf (stance-issue-structure st1) (get-node opp issue))
             st1))
          (else nil))))


(defun eng-stance2 (subject stance)  
  (let* ((side (stance-side stance)))
    (format nil " ~A "
            (case side
              ((pro) (cond ((is-issue? subject)
                            (english-issue-pro subject stance))
                           ((is-record? subject)
                            (english-record-pro subject stance))
                           (else 
                            (english-pro subject stance))))
              ((con) (cond ((is-issue? subject)
                            (english-issue-con subject stance))
                           ((is-record? subject)
                            (english-record-con subject stance))
                           (else
                            (english-con subject stance))))
              ))))


(setf (symbol-function 'say) (symbol-function 'eng-stance))

(defmethod is-issue? (self)
  (or (bill-p self)      ;; added for stating bill positions
      (issue-p self)))

;; ?? need to parameterize for PERSON, TENSE, AFFECT

;;;;------------------------------------------------------------------------
;;;;    issue phrases
;;;;------------------------------------------------------------------------
; 
; (define (eng-stance-issue stance)
;   (english-issue (reveal-issue stance) (stance-side stance)))
; 

(defun eng-stance-issue (stance)  
  (let* ((iss (reveal-issue stance))
         (imp? (<important? 'C (stance-importance stance)))
         (side (stance-side stance))
         (phrase (english-issue-phrase iss side))
         (said  (said? *context* iss))
         (short (english-short iss))
         (iss-eng (if imp?
                      (case side
                        ((pro) (if (sarcasm?)
                                   (issue-con-english iss)
                                   (issue-pro-english iss)))
                        ((con) (if (sarcasm?)
                                   (issue-pro-english iss)
                                   (issue-con-english iss))))
                      nil)))
    (format nil "~A~A"
            (if (and (not said) phrase)
                (concatenate 'string phrase " ")
                "")
            (cond ((and said short))
                  (said
                   (english iss))
                  (iss-eng)
                  (else (english iss)))
            )))
                   

;; the intro phrase is used the first time an issue is mentioned
;; in the current context.

(defun english-issue-phrase (iss side)  
  (case (issue-type iss)    
    ((principle) (gen-pick-one
                  nil
                  "the belief in"
                  "the principle of"))
    ((right)     "the right of")
    ((proposal)  "the proposal of")
    ((policy)    "the policy of")
    ((group)     (case (if (sarcasm?)
                           (flip-side side)
                           side)
                   ((pro) "the legitimate concerns of")
                   ((con) "the special interests of")))
    ((cause)     "the cause of")
    ((program)   "the program of")
    ((nomination) "the nomination and appointment of")
;;    ((reform)    (gen-pick-one
;;                  "efforts to achieve a better"
;;                  "the reform of"))
    ((problem)   "the problem of")
    ((foreign)   (gen-pick-one
                  "American involvement in"
                  "our country's role in"
                  "our country's relations with"))
    (otherwise    nil)))             ;; was "the issue of"
    

;;------------------------------------------------------------------------
;;  pro/con issue statements -- human subjects
;;------------------------------------------------------------------------


(defun get-importance (obj)  
  (cond ((relation-p obj) (relation-importance obj))
        ((stance-p obj)   (stance-importance obj))
        (else nil)))

(defun get-side (obj)  
  (cond ((relation-p obj) (relation-side obj))
        ((stance-p obj)   (stance-side obj))
        (else nil)))

(defun english-pro (subject stance)  
  (case (get-importance stance)
    ((a) (gen-pick-one
          (format nil "~A unwavering in ~A support of"
                  (gen-verb subject "be" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A long-standing support of"
                  (gen-verb subject "stress" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A deep-rooted support of"
                  (gen-verb subject "emphasize" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "unwaveringly ~A" (gen-verb subject "endorse" nil 'present))
          (format nil "completely ~A" (gen-verb subject "support" nil 'present))
          (format nil "~A a champion of" (gen-verb subject "be" nil 'present))
          (format nil "~A a staunch defender of" (gen-verb subject "be" nil 'present))
          (format nil "~A deeply committed to" (gen-verb subject "be" nil 'present))
          (format nil "~A always stood for" (gen-verb subject "have" nil 'present))
          (format nil "~A always felt strongly about" (gen-verb subject "have" nil 'present))
          (format nil "~A always believed in" (gen-verb subject "have" nil 'present))))
         
    ((b) (gen-pick-one
          (format nil "strongly ~A" (gen-verb subject "support" nil 'present))
          (format nil "readily ~A" (gen-verb subject "endorse" nil 'present))
          (format nil "~A a defender of" (gen-verb subject "be" nil 'present))
          (format nil "~A in" (gen-verb subject "believe" nil 'present))
;; problems?   (format nil "~A very concerned about" (gen-verb subject "be" nil 'present))
          (format nil "~A strongly in favor of" (gen-verb subject "feel" nil 'present))
          (format nil "~A deeply about" (gen-verb subject "care" nil 'present))
          (format nil "~A eager to support" (gen-verb subject "be" nil 'present))
          (format nil "~A for" (gen-verb subject "stand" nil 'present))
          (format nil "~A committed to" (gen-verb subject "be" nil 'present))
          (format nil "~A eager to show ~A support for"
                  (gen-verb subject "be" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A strongly in favor of" (gen-verb subject "be" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A of" (gen-verb subject "approve" nil 'present))
          (format nil "~A" (gen-verb subject "endorse" nil 'present))
          (format nil "~A for" (gen-verb subject "be" nil 'present))
;; problems?   (format nil "~A concerned about" (gen-verb subject "be" nil 'present))
          (format nil "~A about" (gen-verb subject "care" nil 'present))
          (format nil "~A" (gen-verb subject "support" nil 'present))
          (format nil "~A in favor of" (gen-verb subject "be" nil 'present))))))


(defun english-con (subject stance)  
  (case (get-importance stance)
    ((a) (gen-pick-one
          (format nil "~A unwavering in ~A opposition to"
                  (gen-verb subject "be" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A of"
                  (gen-verb subject "be" nil 'present)
                  (if (singular? subject)
                      "a longtime opponent"
                      "longtime opponents"))
          (format nil "~A adamantly opposed to" (gen-verb subject "be" nil 'present))
          (format nil "~A till ~A dying day"
                  (gen-verb subject "oppose" nil 'future)
                  (gen-np subject 'poss))
          (format nil "~A never supported" (gen-verb subject "have" nil 'present))))
         
    ((b) (gen-pick-one
          (format nil "~A ~A of"
                  (gen-verb subject "be" nil 'present)
                  (if (singular? subject)
                      "an opponent"
                      "opponents"))
          (format nil "~A an opponent of" (gen-verb subject "be" nil 'present))
          (format nil "~A strongly opposed to" (gen-verb subject "be" nil 'present))
          (format nil "strongly ~A" (gen-verb subject "oppose" nil 'present))
          (format nil "firmly ~A" (gen-verb subject "oppose" nil 'present))
          (format nil "~A strongly against" (gen-verb subject "be" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A opposed to" (gen-verb subject "be" nil 'present))
          (format nil "~A" (gen-verb subject "oppose" nil 'present))
          (format nil "~A to" (gen-verb subject "object" nil 'present))
          (format nil "~A against" (gen-verb subject "be" nil 'present))))))


;;------------------------------------------------------------------------
;;  pro/con statements -- issue subjects
;;------------------------------------------------------------------------

(defun english-issue-pro (subject stance)  
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "~A an essential element of" (gen-verb subject "be" nil 'present))
          (format nil "~A always part of" (gen-verb subject "be" nil 'present))
          (format nil "~A of fundamental importance for" (gen-verb subject "be" nil 'present))))

    ((b) (gen-pick-one
          (format nil "~A" (gen-verb subject "uphold" nil 'present))
          (format nil "~A" (gen-verb subject "reinforce" nil 'present))
          (format nil "~A important for" (gen-verb subject "be" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A compatible with" (gen-verb subject "be" nil 'present))
          (format nil "~A consistent with" (gen-verb subject "be" nil 'present))))))


(defun english-issue-con (subject stance)  
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "strongly ~A" (gen-verb subject "oppose" nil 'present))
          (format nil "~A completely against" (gen-verb subject "stand" nil 'present))))
;;;;          (format nil "~A completely incompatible with" (gen-verb subject "be" nil 'present))

    ((b) (gen-pick-one
          (format nil "~A" (gen-verb subject "challenge" nil 'present))
          (format nil "~A" (gen-verb subject "oppose" nil 'present))
          (format nil "~A firmly against" (gen-verb subject "stand" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A issue with" (gen-verb subject "take" nil 'present))
          (format nil "~A" (gen-verb subject "question" nil 'present))
          (format nil "~A against" (gen-verb subject "stand" nil 'present))
          (format nil "~A in opposition to" (gen-verb subject "be" nil 'present))))))
;;;;          (format nil "~A inconsistent with" (gen-verb subject "be" nil 'present))




;;;;------------------------------------------------------------------------
;;;;    test code
;;;;------------------------------------------------------------------------
;;
;; Generates an English sentence for a set of issues using either
;; the norm, or a dummy stance.
;;
;; E.g., 
;; (eng-test 'pro 'b '(aids women abortion))
;;  
;;  ==>
;;    
;;  [()] the average American cares about the issue of AIDS research and education.
;;  [RIGHT] popular opinion approves of the right Women's Rights (*).
;;  [()] the typical American cares about the issue of a woman's right to have an abortion.
;;  
(defun eng-test (side imp iss-list)  
  (let ((issues (mapcar 
                 #'(lambda (iss)
                   (if (issue-p iss)
                       iss
                       (get-node iss issue)))
                 iss-list)))
    (mapcar 
     #'(lambda (iss)
       (let ((type (if (issue-type iss)
                       (format nil " [~A]" (issue-type iss))
                       "")))
         (cond ((issue-norm iss)
                (format nil "~A"
                        (say (gen-norm-np) (issue-norm iss))
                        ))
               (else " ")
               ;;; removed printing of issue-type ... 6/14/90
       ;;; pruned this option ...
               (t
                (format nil "~%Not a norm...~%~A ~A"
                        (say *self*
                             (init-stance (list iss side imp (get-node 'aclu group))))
                        type)))))
     issues)))


;; specific code for testing norms
;;
;; E.g.,
;; (map norm-test (collect (db-all issue) issue-norm))
;;
(defun norm-test (obj)  
  (cond ((norm? obj)
         (say (gen-norm-np) obj))
        ((group-p obj)
         (eng-test-group (gen-pick-one 'pro 'con)
                         (gen-pick-one 'a 'b 'c)
                         (list obj)))
        ((issue-p obj)
         (eng-test (gen-pick-one 'pro 'con)
                   (gen-pick-one 'a 'b 'c)
                   (list obj)))
        (else nil)))


(defun gen-group-stances (g)  
  (let* ((grp (if (group-p g)
                  g
                  (get-node g group)))
         (stances (group-stances grp)))
    (init-context)
    (paragraph (mapcar #'(lambda (st) (eng-stance grp st)) stances)
               so)))

    


;;;;------------------------------------------------------------------------
;;;;    say pro/con reasons for issues
;;;;------------------------------------------------------------------------


(defun say-reasons (subj reasons)  
  (paragraph
   (mapcar 
    #'(lambda (reason)
      (say subj reason))
    reasons)
   so))

(defun say-p/c (iss stream)  
  (let* ((i (if (issue-p iss)
                iss
                (get-node iss issue)))
         (pro (issue-pro-stances i))
         (con (issue-con-stances i)))
    (init-context)
    (if pro
        (say-side i 'pro pro stream))
    (if con
        (say-side i 'con con stream))
    (say-opposite iss stream)
    (values)))


(defun say-side (iss side reasons stream)  
  (let ((phrase (case side
                  ((pro) "support of ")
                  ((con) "opposition to "))))
    (say-cluster-reasons (proto-dnp issue-np
                            'number 'sing
                            'issue t
                            'lex (concatenate 'string phrase (gen-np iss 'obj)))
                         reasons
                         stream)))



(defun say-opposite (iss stream)  
  (cond ((issue-opposite iss)
         (paragraph
          (concatenate 'string 
                       "the opposite of "
                       (gen-np iss 'subj)
                       " is "
                       (english (get-node (issue-opposite iss) issue))
                       ".")
          stream)))
  (values))

;;;;------------------------------------------------------------------------
;;;;    End of English.t
;;;;------------------------------------------------------------------------
