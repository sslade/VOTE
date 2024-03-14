
;;;;------------------------------------------------------------------------
;;;;    nouns
;;;;------------------------------------------------------------------------

;;  
;;  plurals
;;  pronouns
;;  

;;;;------------------------------------------------------------------------
;;;;    plurals
;;;;------------------------------------------------------------------------

;;  (gen-plural noun)  returns a string with the plural form of the noun.
;;  An exception list is bound to *plural-alist*
;;  
;;  man ==> men
;;  self ==> selves
;;  
;;  dress ==> dresses
;;  church ==> churches
;;  
;;  toddy ==> toddies
;;  honey ==> honeys
;;  
;;  dog ==> dogs
;;  cat ==> cats
;;  
;;  

(defun gen-plural (noun)  
  (let* ((eses '(#\s #\S #\z #\Z #\x #\X))
         (vowels '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
         (plural (assoc (->symbol noun) *plural-alist*))
         (reverse-noun (reverse (coerce noun 'list)))
         (last-letter (car reverse-noun))
         (penult (cadr reverse-noun)))
    (cond (plural (->string (cdr plural)))
          ((or (member last-letter eses)
               (and (member last-letter '(#\h #\H))
                    (member penult '(#\c #\C #\s #\S))))
           (concatenate 'string noun "es"))
          ((and (member last-letter '(#\y #\Y))
                (not (member penult vowels)))
           (concatenate 'string 
            (coerce (reverse (cdr reverse-noun)) 'string)
            "ies"))
          (else
           (concatenate 'string 
            noun "s")))))
         

;; special plurals -- exceptions

(setf *plural-alist*   
  '((person . people)
    (leaf . leaves)
    (child . children)
    (fish . fish)
    (deer . deer)
    (sheep . sheep)
    (goose . geese)
    (ox . oxen)
    (man . men)
    (woman . women)
    (self . selves)))


;;;;------------------------------------------------------------------------
;;;;   noun phrases
;;;;------------------------------------------------------------------------

(defun gen-np (obj case)  
  (cond ((is-issue? obj)
         (cond ((and (said? *context* obj) (english-short obj)))
               (else (english obj))))             
        ((use-pronoun? *context* obj)
         (gen-pronoun obj case))
        ((eq case 'poss)
         (use-noun *context* obj)
         (->possessive (gen-np obj 'subj)))
        ((and (said? *context* obj)
              (english-short obj)))
        ((english obj))
        (else nil)))


(defun ->possessive (np-str)  
  (let ((eses '("s" "S" "z" "Z"))
        (last-letter (string-last np-str)))
    (concatenate 'string np-str
                   (if (member last-letter eses :test #'string=)
                       "'"
                       "'s"))))

;; not too efficient, but what the hell...
(defun string-last (str)  
  (subseq str (1- (length str))))

(defun gen-np-list (np-lst case)  
  (cond ((null np-lst) "")
        ((null (cdr np-lst))
         (concatenate 'string "and "
                        (gen-np (car np-lst) case)))
        (else
         (concatenate 'string (gen-np (car np-lst) case)
                        ", "
                        (gen-np-list (cdr np-lst) case)))))

(defun test-groups (g-lst)  
  (mapcar #'(lambda (g) (get-node g group))
       g-lst)
  )

;;;;------------------------------------------------------------------------
;;;;   pronouns
;;;;------------------------------------------------------------------------

(defun xgen-pronoun (object case)  
  (let ((gender (gen-get-feature object 'gender))
        (number (gen-get-feature object 'number))
        (person (gen-get-feature object 'person))
        (human  (gen-get-feature object 'human)))
    (gen-pronoun2 (list gender number person case human))))

;; updated 3/9/2022 by SBS

(defun gen-pronoun (object case)  
  (let ((gender 'male)
        (number 'sing)
        (person 'third)
        (human  't))
    (gen-pronoun2 (list gender number person case human))))


;;    features: (gender number person case human?)

(defun gen-pronoun2 (features)  
  (cond ((cdr (assoc features *pronoun-alist* :test #'equalp)))
        ((cdr (assoc (cons 'unspec (cdr features)) *pronoun-alist* :test #'equalp)))
        (else nil)))

;;  
;;   pronoun-alist is of the form:
;;    ((gender number person case human) . <pronoun>)
;;    it is used by gen-pronoun2
;;  

(setf *pronoun-alist*     
  '(((unspec sing first  subj    t)  . "I")
    ((unspec sing second subj    t)  . "you")
    ((  male sing third  subj    t)  . "he")
    ((female sing third  subj    t)  . "she")
    ((unspec sing third  subj    t)  . "it")
    ((unspec sing third  subj  nil)  . "it")
    
    ((unspec sing first  obj     t)  . "me")
    ((unspec sing second obj     t)  . "you")
    ((  male sing third  obj     t)  . "him")
    ((female sing third  obj     t)  . "her")
    ((unspec sing third  obj     t)  . "it")
    ((unspec sing third  obj   nil)  . "it")

    ((unspec sing first  poss    t)  . "my")
    ((unspec sing second poss    t)  . "your")
    ((  male sing third  poss    t)  . "his")
    ((female sing third  poss    t)  . "her")
    ((unspec sing third  poss    t)  . "its")
    ((unspec sing third  poss  nil)  . "its")

    ((unspec plur first  subj    t)  . "we")
    ((unspec plur second subj    t)  . "you")
    ((unspec plur third  subj    t)  . "they")
    
    ((unspec plur first  obj     t)  . "us")
    ((unspec plur second obj     t)  . "you")
    ((unspec plur third  obj     t)  . "them")
    
    ((unspec plur first  poss    t)  . "our")
    ((unspec plur second poss    t)  . "your")
    ((unspec plur third  poss    t)  . "their")  ))



;;;;------------------------------------------------------------------------
;;;;   gen-get-feature
;;;;------------------------------------------------------------------------

(defun gen-get-feature (object feature)  
  (cond ;;((get object feature))
        ((and (instance-p object)
              (member feature (all-the-slots object))
              (slot-value object feature)))
        ((instance-p object)
         (let ((np (get-np (class-name (class-of object)))))
           (if np
             (gen-get-feature np feature)
             'unspec)))
        (else 'unspec)))


(defun singular? (obj)  
  (eq (gen-get-feature obj 'number) 'sing))
