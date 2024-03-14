

;;;;------------------------------------------------------------------------
;;;;    noun phrase structure type
;;;;------------------------------------------------------------------------


(defclass np ()
  ((person :initform nil :accessor np-person :documentation "first second or third")
   (number :initform nil :accessor np-number :documentation "sing or plur")
   (human  :initform nil :accessor np-human  :documentation "t or nil")
   (gender :initform nil :accessor np-gender :documentation "male female or unspec")
   (issue  :initform nil :accessor np-issue  :documentation "t or nil for issues")
   (bill   :initform nil :accessor np-bill   :documentation "t or nil for bills")
   (record :initform nil :accessor np-record :documentation "t or nil for previous bills -- the record")
   (name   :initform nil :accessor np-name   :documentation "?")
   (db     :initform nil :accessor np-db     :documentation "data base associated with this np")
   (lex    :initform nil :accessor np-lex    :documentation "English phrase for self")
   (lex-list :initform nil :accessor np-lex-list :documentation "list of english phrases for self")
   (use-noun :initform nil :accessor np-use-noun :documentation "flag: t if never to use a pronoun")
   (use-pronoun :initform nil :accessor np-use-pronoun :documentation "flag: t if always to use a pronoun"))
  (:documentation "class for noun phrases"))

; (defpredicate np)

(defmethod all-the-slots ((self np))
  '(person number human gender issue bill record name 
    db lex lex-list use-noun use-pronoun))

 
(defmethod print-object ((self np) stream)
  (format stream "#<NP (~A) ~A>"
          (object-hash self) 
          (np-name self)))


(defgeneric is-record? (self)
  (:method (self) nil)
  (:method ((self np)) (np-record self)))

 
(defmethod is-issue? ((self np))
  (or (np-bill self)
      (np-issue self)))

(defmethod english ((self np))
  (or (np-lex self)
      (apply #'gen-pick-one (np-lex-list self))))

(defmethod pretty-print ((self np) stream)
  (mapc 
   #'(lambda (slot)
       (cond ((null (slot-value self slot))
              nil)
             (else
              (format stream "~%~A: ~15T~A"
                      (capitalize slot)
                      (slot-value self slot)))))
   (all-the-slots self)))

(setf np-table (make-hash-table))
; (make-executable np-table)
(setf (symbol-function 'np-table)
      #'(lambda (key) (gethash key np-table)))
(defun (setf np-table) (val key) 
  (setf (gethash key np-table) val))


(defun get-np (val) (np-table val)) 

(defun proto-dnp (proto &rest l)  
  (let ((np (copy-instance proto)))
    (dnp2 np l)
    np))


(defun dnp (name &rest l)  
  (let ((np (make-instance 'np)))
    (setf (np-name np) name)
    (setf (np-table name) np)
    (set name np)
    ;;   (myeval `(define ,name (get-np ',name)))
    (dnp2 np l)
    (let ((db (np-db np)))
      (setf (np-table (*db-table* db)) np)
      (setf (np-table db) np)
      np)))

(defun dnp2 (np l)  
  (cond ((null l) np)
        ((car l)
         (setf (slot-value np (car l)) (cadr l))
         (dnp2 np (cddr l)))
        (else
         (dnp2 np (cddr l)))))

(defmacro dnpq (&rest l)  
     `(apply #'dnp (quote ,l)) )

(dnpq member-np
      person third
      number sing
      human t
      db member
      gender male)

(dnpq issue-np
      person third
      number sing
      db issue
      gender unspec
      human nil)

(dnpq group-np
      person third
      number sing
      db group
      gender unspec
      human t)

(dnpq bill-np
      person third
      number sing
      db bill
      gender unspec
      human nil)

(dnpq bill-for-np
      person third
      number sing
      gender unspec
      human  nil
      lex    "this bill")
      
(dnpq bill-agn-np
      person third
      number plur
      gender unspec
      human  t
      lex    "opponents of this bill")
      

(dnpq *record*
      person third
      number sing    
      gender unspec
      record t
      human t
      lex-list  ("the record"
                 "the voting record")
      )

(dnpq district-np
      person third
      number sing
      gender unspec
      human t
      lex    "my district")

(dnpq *self*
      human t
      person first
      number sing
      gender male
      lex    "present company"
      use-pronoun always)

(dnpq *norm*
      person third
      number sing    ;; !! not plural !!
      gender unspec
      human t
      use-noun always
      lex-list  (
                 "popular opinion"
                 "the public in general"
                 "the citizenry"
                 "the populace"
                 "the general population"
                 "the American public"
                 "the general public"
                 )
      )


(defvar *norm-plur*   (proto-dnp *norm*
             'number 'plur
             'lex-list '(
                         "the majority of people"
                         "most Americans"
                         "most voters"
                         "most citizens"
                         "most taxpayers"
                         "people in general"
                         "average people"
                         "normal people"
                         "most people"
                         )))

(defvar *norm-male*   (proto-dnp *norm*
             'gender 'male
             'lex-list '(
                         "the man on the street"
                         "any right-thinking person"
                         "the common working person"
                         "the average citizen"
                         "the normal citizen"
                         "the typical citizen"
                         "your average taxpayer"
                         "the average person"
                         "the average voter"
                         "the average American"
                         "the typical voter"
                         "the typical American"
                         "a normal person"
                         "a normal American"
                         )))

                         ;; "your average Joe"
                         ;; "John Q. Public"
           
;; Twice as likely to pick male as other norms

(defun gen-norm-np ()
  (gen-pick-one
   *norm*
   *norm-male*
   *norm-male*
   *norm-plur*
   ))

(defun verb-test-person (person number gender)  
  (proto-dnp *human*
             'person person
             'number number
             'gender gender))


(dnpq *human*
      human t
      use-pronoun always)
