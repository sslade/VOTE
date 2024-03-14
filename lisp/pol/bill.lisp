
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

(defclass bill (record)
  ((name   :initform nil :documentation "name of bill" :accessor bill-name) 
   (english  :initform nil :documentation "English version of bill name" :accessor bill-english)
   (french   :initform nil :documentation "French version of bill name" :accessor bill-french)
   (bnumber  :initform nil :documentation "Bill number" :accessor bill-bnumber)
   (sort-key :initform nil :documentation "Bill Sort Key" :accessor bill-sort-key)
   (synonyms :initform nil :documentation "Bill synonyms - list of symbols" :accessor bill-synonyms)
   (date-of-vote :initform nil :documentation "Bill Vote date" :accessor bill-date-of-vote)
   (vote-tally :initform nil  :documentation "Vote tally list" :accessor bill-vote-tally)
   (pres-pos  :initform nil :documentation "President's position FOR or AGN" :accessor bill-pres-pos)
   (test-bill  :initform nil :documentation "Flag for test bills" :accessor bill-test-bill)
   (session   :initform nil :documentation "Session of Congress" :accessor bill-session)
   (isa       :initform nil :documentation "List of symbols" :accessor bill-isa)
   (isa-depth  :initform nil :documentation "String for padding printing" :accessor bill-isa-depth)
   (instances  :initform nil :documentation "List of bills" :accessor bill-instances)
   (majority-factor :initform nil 
                    :documentation "Number used for calculating votes required for passage"
                    :accessor bill-majority-factor)
   (issues    :initform nil :documentation "List of symbols" :accessor bill-issues)
   (importance  :initform nil :documentation "Intrinsic importance of bill" :accessor bill-importance)
   (stance-for :initform nil :documentation "What voting for this bill implies" 
               :accessor bill-stance-for)
   (stance-agn :initform nil :documentation "What voting against this bill implies" 
               :accessor bill-stance-agn)
   (i-stance-for :initform nil 
                 :documentation "What voting for this bill implies (inferred from remarks)" 
                 :accessor bill-i-stance-for)
   (i-stance-agn :initform nil 
                 :documentation "What voting against this bill implies (inferred from remarks)" 
               :accessor bill-i-stance-agn)
   (notes     :initform nil :documentation "List of remarks" :accessor bill-notes))
  (:documentation "Bill  Class"))

; (defpredicate bill)

(defmethod all-the-slots ((obj bill))
  (append '(name english french bnumber sort-key synonyms date-of-vote vote-tally 
            pres-pos test-bill session isa isa-depth instances majority-factor 
            issues importance stance-for stance-agn i-stance-for i-stance-agn notes)
          (call-next-method)))


(defmethod print-object ((self bill) stream)
  (format stream "#{Bill (~A) ~A}"
          (object-hash self) 
          (bill-name self)))
   
(defmethod english ((self bill))
  (cond (*english*
         (or (bill-english self)
             (concatenate 'string "the " (bill-name self))))
        ((bill-french self))
        (else
         (concatenate 'string "**the " (bill-name self) "**"))))


(defmethod english-short ((self bill))
  (bill-bnumber self))

(defmethod inorder? ((self bill) (other bill))
  (if (bill? other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (cond ((and (date? first) (date? second))
             (inorder? first second))
            (else
             (string< first second))))
    nil))

(defmethod synonyms ((self bill))
  (let ((name (read-string->list (bill-name self))))
    (if (= (length name) 1)
      (append name (bill-synonyms self))
      (bill-synonyms self))))

(defmethod sort-key ((self bill))
  (if (bill-sort-key self)
    (bill-sort-key self)
    (bill-name self)))

(defmethod set-isa-sort ((self bill))
  (setf (bill-isa-depth self) (isa-depth self bill))
  (setf (bill-sort-key self) (generate-isa-sort-key self bill)))

(defmethod set-alpha-sort ((self bill))
  (setf (bill-isa-depth self) "")
  (setf (bill-sort-key self) (bill-name self)))

(defmethod set-date-sort ((self bill))
  (setf (bill-isa-depth self) "")
  (setf (bill-sort-key self) (bill-date-of-vote self)))

(defmethod id ((self bill))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'bill)))
        (else (record-symbol self))))
   
(defmethod print-readable2 ((self bill) stream)
  (format stream "~%(get-node ~A bill)"
          (id self)))

(defmethod print-header ((self bill) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~A~A~18T~A~24T[~A]~A ~A"
          (case (char (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (if (bill-isa-depth self)
            (bill-isa-depth self)
            "")
          (bill-bnumber self)
          (bill-session self)
          (bill-importance self)
          (if (bill-test-bill self)
            "*"
            " ")
          (bill-name self)))
   

(defmethod print-hc ((self bill) stream &rest args)
    (declare (ignore args))
    (format stream "~%~A: [~A] ~15T~A~%"
            (bill-bnumber self)
            (bill-importance self)
            (bill-name self))
    (format stream "~2TSession: ~A, Date: ~A~%"
            (bill-session self)
            (let ((date (bill-date-of-vote self)))
              (if (date? date)
                  (format nil "~A/~A/~A"
                          (date-month date)
                          (date-day date)
                          (date-year date))
                  "-none-")))
    (format stream "~2TVote tally: ~A"
            (bill-vote-tally self))
    (mapc
     #'(lambda (sel-id)
       (let ((selector (car sel-id))
             (identifier (cdr sel-id)))
         (if (funcall selector self)
             (progn 
               (format stream "~%~A~26T"
                       identifier)
               (pretty-print (funcall selector self) stream)
               ))))
     (list
;;      (cons synonyms "Names:")
;;      (cons bill-isa "ISA:")
;;      (cons bill-issues "Issues:")
;;      (cons bill-keywords "Keywords:")
      (cons #'bill-stance-for "Stance-FOR:")
      (cons #'bill-stance-agn "Stance-AGN:")
      (cons #'bill-i-stance-for "I-Stance-FOR:")
      (cons #'bill-i-stance-agn "I-Stance-AGN:")
      ))

    (mapc 
     #'(lambda (str)
;;;       (format stream "~2T~A~%" (fix-latex-dollar str)))
       (format stream "~2T~A~%" str))
     (remarks self))
    (terpri stream)
    (say-bill-p/c self stream)
    )
             

(defmethod pretty-print ((self bill) stream)
  (call-next-method)
  (cond ((bill-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (bill-notes self))))
  (say-bill-p/c self stream))



(defmethod input-prototype ((self bill) slot)
  (case slot
    ((isa-depth)     #'(LAMBDA () ""))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((bnumber)       #'(LAMBDA () (prompted-string-input "Bill Number: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((date-of-vote)  #'(LAMBDA () (prompted-date-input "Date of vote: ")))
    ((vote-tally)    #'(LAMBDA () (prompted-list-input "Vote Tally: ")))
    ((session)       #'(LAMBDA () (prompted-string-input "Session: ")))
    ((issues)        #'(LAMBDA () (prompted-list-input "Issues: ")))
    ((pres-pos)      #'(LAMBDA () (prompted-symbol-input "Pres-Pos (FOR/AGN): ")))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    (otherwise       (call-next-method))))

 

(defmethod update-prototype ((self bill) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    ((french)        #'(LAMBDA () (prompted-string-input "French: ")))
    ((bnumber)       #'(LAMBDA () (prompted-string-input "Bill Number: ")))
    ((date-of-vote)  #'(LAMBDA () (prompted-date-input "Date of vote: ")))
    ((vote-tally)    #'(LAMBDA () (prompted-list-input "Vote Tally: ")))
    ((pres-pos)      #'(LAMBDA () (prompted-symbol-input "Pres-Pos (FOR/AGN): ")))
    ((session)       #'(LAMBDA () (prompted-string-input "Session: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((majority-factor) #'(LAMBDA () (prompted-symbol-input "Majority Factor: ")))
    ((issues)        #'(LAMBDA () (prompted-list-input "Issues: ")))
    ((test-bill)     #'(LAMBDA () (prompted-symbol-input "Test bill?: ")))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    ((stance-for)    #'(LAMBDA () (prompted-stance-list-input "Stances FOR this bill: " (bill-bnumber self) 'bill)))
    ((stance-agn)    #'(LAMBDA () (prompted-stance-list-input "Stances AGAINST this bill: " (bill-bnumber self) 'bill)))
    ((i-stance-for)  #'(LAMBDA () (infer-bill-stances self 'for)))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (bill-notes self))))
    (otherwise       (call-next-method))))
 

(defmethod decision-dependencies ((self bill))
  '(stance-for stance-agn importance))

(defmethod no-print-readable-fields ((self bill))
  (append '(sort-key instances isa-depth i-stance-for i-stance-agn) (call-next-method)))

(defmethod table-index-fields ((self bill))
  (append '(synonyms symbol bnumber isa keywords issues) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of Bills
;;----------------------------------------------------------

;; bill is defvar'd in db.lisp
(setq bill (make-db)) 
(setf (db-name bill) 'bill) 
(setf (db-prompt bill) "BILL> ")
(setf (db-class bill) 'bill)
(setf (db-commands bill)
      (append
       '((notes (notes note))
         (name (name))
         (english (english))
         (french (french))
         (bnumber (number bnumber))
         (date-of-vote (date-of-vote vote-date))
         (pres-pos (pres-pos))
         (vote-tally (vote-tally tally vote))
         (test-bill (test-bill))
         (session (session))
         (synonyms (synonyms synonym names))
         (isa (isa))
         (majority-factor (majority-factor majority factor))
         (issues (issues issue))
         (importance (importance import))
         (stance-for (for stance-for))
         (stance-agn (agn stance-agn))
         (i-stance-for (infer-stances i-stance-for)))
       *record-db-commands*))

(init-db bill)


