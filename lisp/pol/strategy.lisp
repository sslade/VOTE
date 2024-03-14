
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

(defclass strategy (record)
  ((name    :initform nil :accessor strategy-name :documentation "string")
   (sort-key :initform nil :accessor strategy-sort-key :documentation "string")
   (keywords :initform nil :accessor strategy-keywords :documentation "list of symbols")
   (synonyms :initform nil :accessor strategy-synonyms :documentation "list of symbols")
   (isa      :initform nil :accessor strategy-isa      :documentation "list of symbols")
   (isa-depth :initform nil :accessor strategy-isa-depth :documentation "string for padding printing.")
   (instances :initform nil :accessor strategy-instances :documentation "list of strategies")
   (polarity  :initform nil :accessor strategy-polarity  :documentation "intrinsic position for this version of the strategy")
   (quote     :initform nil :accessor strategy-quote     :documentation "real quotation")
   (notes     :initform nil :accessor strategy-notes     :documentation "list of remarks")
   (rank      :initform nil :accessor strategy-rank      :documentation "selection order for this strategy")
   (test      :initform nil :accessor strategy-test      :documentation "predicate for testing applicability of this strategy [text]")
   (test-code :initform nil :accessor strategy-test-code :documentation "predicate code")
   (preamble  :initform nil :accessor strategy-preamble  :documentation "preamble English generation procedure")
   (protocol  :initform nil :accessor strategy-protocol  :documentation "procedure for generating transcript comments")
   (example   :initform nil :accessor strategy-example   :documentation "code to execute to test this strategy")
   (no-second-try :initform nil :accessor strategy-no-second-try :documentation "flag -- don't execute this strategy at deep analysis")
   )
  (:documentation "a class for strategies"))

; (defpredicate strategy)


(defmethod all-the-slots ((obj strategy))
  (append '(name sort-key keywords synonyms isa isa-depth instances polarity 
            quote notes rank test test-code preamble protocol example no-second-try)
          (call-next-method)))

(defmethod print-object ((self strategy) stream)
    (format stream "#{Strategy (~A) ~A}"
	    (object-hash self) 
	    (strategy-name self)))
   

(defmethod inorder? ((self strategy) (other strategy))
  (if (strategy? other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))



(defmethod synonyms ((self strategy))
  (let ((name (read-string->list (strategy-name self))))
    (if (= (length name) 1)
      (append name (strategy-synonyms self))
      (strategy-synonyms self))))


(defmethod sort-key ((self strategy))
  (if (strategy-sort-key self)
    (strategy-sort-key self)
    (strategy-rank self)))


(defmethod set-isa-sort ((self strategy))
  (setf (strategy-isa-depth self) (isa-depth self strategy))
  (setf (strategy-sort-key self) (generate-isa-sort-key self strategy)))

;; alpha within rankings

(defmethod set-alpha-sort ((self strategy))
  (setf (strategy-isa-depth self) "")
  (setf (strategy-sort-key self) (concatenate 'string (strategy-rank self)
                                              (strategy-name self))))

(defmethod id ((self strategy))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'strategy)))
        (else (record-symbol self))))


(defmethod print-readable2 ((self strategy) stream)
  (format stream "~%(get-node ~A strategy)"
          (id self)))

(defmethod print-header ((self strategy) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~A~A~50T[~A]~55T~A~A~A~A"
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
          (strategy-isa-depth self)
          (strategy-name self)
          (strategy-rank self)
          (if (strategy-test-code self)
            "@"
            " ")
          (if (strategy-preamble self)
            "@"
            " ")
          (if (strategy-protocol self)
            "@"
            " ")
          (cond ((strategy-synonyms self))
                (else ""))))


(defmethod print-hc ((self strategy) stream &rest args)
  (declare (ignore args))
  (format stream "~%<H2>~A</h2>~%"
          (strategy-name self))
  (format stream "<DL>~%<DT>Rank:<DD>~15T~A~%"
          (strategy-rank self))   
  (mapc 
   #'(lambda (slot-id)
       (let ((slot (car slot-id))
             (identifier (cdr slot-id)))
         (if (slot-value self slot)
           (progn 
             (format stream "<DT>~A<DD> "
                     identifier)
             (mapc
              #'(lambda (str)
                  (format stream "~15T~A~%" str))
              (slot-value self slot))))))
   '((test "Test:")
     (remarks "Remarks:")
     (quote "Quote:")))
  (format stream "</DL>~%")
    )
   

(defmethod pretty-print ((self strategy) stream)
  (mapc 
   #'(lambda (slot)
       (let ((value (slot-value self slot)))
         (cond ((null value)
                nil)
              ((member slot (no-pp-fields self))
                nil)
              ((eq slot 'db)
               (format stream "~%Db: ~15T")
               (print value stream))
              ((eq slot 'notes)
               (format stream "~%Notes: ~15T********")
               (mapc 
                #'(lambda (note)
                    (pretty-print note stream))
                value))
              ((member slot '(quote remarks test))
               (format stream "~%~A:" (capitalize slot))
               (mapc 
                #'(lambda (str)
                    (format stream "~15T~A~%" str))
                value))
              ((eq slot 'instances)
               (format stream "~%~A:" (capitalize slot))
               (mapc
                #'(lambda (node)
                    (format stream "~15T~A~%" node))
                value))
              (else
               (format stream "~%~A: ~15T"
                       (capitalize slot))
               (pretty-print value stream)))))
   (all-the-slots self)))
  

(defmethod input-prototype ((self strategy) slot)
    (case slot
      ((isa-depth)     #'(LAMBDA () ""))
      ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
      ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
      ((test)          #'(LAMBDA () (prompted-string-list-input "Test: ")))
      ((quote)         #'(LAMBDA () (prompted-string-list-input "Quote: ")))
      ((rank)          #'(LAMBDA () (prompted-string-input "Rank: ")))
      (otherwise       (call-next-method))))


(defmethod update-prototype ((self strategy) slot)
  (case slot
    ((name)          #'(lambda () (prompted-string-input "Name: ")))
    ((quote)         #'(lambda () (prompted-string-list-input "Quote: ")))
    ((synonyms)      #'(lambda () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(lambda () (prompted-list-input "ISA: ")))
    ((rank)          #'(lambda () (prompted-string-input "Rank: ")))
    ((test)          #'(lambda () (prompted-string-list-input "Test: ")))
    ((test-code)     #'(lambda () (prompted-s-exp-input "Test-code: ")))
    ((no-second-try) #'(lambda () (prompted-symbol-input "No second try?: ")))
    ((preamble)      #'(lambda () (prompted-s-exp-input "Preamble procedure name: ")))
    ((protocol)      #'(lambda () (prompted-s-exp-input "Protocol procedure name: ")))
    ((example)       #'(lambda ()
                         (let ((code (strategy-example self)))
                           (cond ((and code
                                       (prompted-yes-no-input "Evaluate current example? "))
                                  (eval code)
                                  code)
                                 (else
                                  (prompted-s-exp-input "Example: "))))))
    ((polarity)      #'(lambda () (prompted-string-input "Polarity: ")))
    ((notes)         #'(lambda () (cons (input-values (make-instance 'note))
                                        (strategy-notes self))))
    (otherwise       (call-next-method))))


(defmethod no-print-readable-fields ((self strategy))
  (append '(instances sort-key isa-depth) (call-next-method)))

(defmethod table-index-fields ((self strategy))
  (append '(synonyms isa symbol) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of Strategies
;;----------------------------------------------------------


(setf strategy (make-db))
(setf (db-name strategy) 'strategy) 
(setf (db-prompt strategy) "STRATEGY> ")
(setf (db-class strategy) 'strategy)
(setf (db-commands strategy)
      (append
       '((quote (quote))
         (notes (notes note))
         (name (name))
         (synonyms (synonyms synonym names))
         (test (test))
         (test-code (test-code code))
         (protocol (protocol))
         (preamble (preamble))
         (no-second-try (no-second-try no-second))
         (example (example))
         (rank (rank))
         (isa (isa))
         (polarity (polarity polar)))
       *record-db-commands*)
       )

(init-db strategy)


