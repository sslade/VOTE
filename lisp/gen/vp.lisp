
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

(defclass vp (record)
  ((side       :initform nil :accessor vp-side)
   (importance :initform nil :accessor vp-importance)
   (type       :initform nil :accessor vp-type :documentation "symbol --  (person/issue/record)")
   (index-key  :initform nil :accessor vp-index-key :documentation "string -- type+side+importance")
   (phrase     :initform nil :accessor vp-phrase)
   (infinitive :initform nil :accessor vp-infinitive)
   (tense      :initform nil :accessor vp-tense)
   (passive    :initform nil :accessor vp-passive :documentation "flag")   
   (mark       :initform nil :accessor vp-mark :documentation "gensym used for avoiding repetition")
   (no-negate  :initform nil :accessor vp-no-negate :documentation "flag if phrase not to be negated")
   (pronouns   :initform nil :accessor vp-pronouns)
   (synonyms   :initform nil :accessor vp-synonyms)
   (notes      :initform nil :accessor vp-notes :documentation "list of remarks"))
  (:documentation "a class for verb phrases"))

; (defpredicate vp)

(defmethod all-the-symbols ((self vp))
  (append '(side importance type index-key phrase infinitive
            tense passive mark no-negate pronouns notes)
          (call-next-method)))

(defmethod print-object ((self vp) stream)
  (format stream "#{Vp (~A) ~A}"
          (object-hash self) 
          (vp-index-key self)))
   
(defmethod inorder? ((self vp) other)
    (if (vp-p other)
        (let ((first (sort-key self))
              (second (sort-key other)))
           (string< first second))
        nil))

(defmethod synonyms ((self vp))
  (let ((name (read-string->list (vp-phrase self))))
    (if (= (length name) 1)
      (append name (vp-synonyms self))
      (vp-synonyms self))))

(defmethod sort-key ((self vp))
  (setf (vp-index-key self)
        (format nil "~A-~A-~A"
                (vp-type self)
                (vp-side self)
                (vp-importance self)))
  (vp-index-key self))

(defmethod id ((self vp))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'vp)))
        (else (record-symbol self))))
   
(defmethod print-readable2 ((self vp) stream)
  (format stream "~%(get-node ~A vp)"
          (id self)))

(defmethod print-header ((self vp) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~A~23T~A~65T~A"
          (case (char (status self) 0)       ; 
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (sort-key self)
          (vp-phrase self)
          (vp-infinitive self)))
   
(defmethod print-hc ((self vp) stream &rest args)
  (declare (ignore args))
  (pretty-print self stream))

(defmethod pretty-print ((self vp) stream)
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
             
               ((eq slot 'remarks)
                (format stream "~%~A:" (capitalize slot))
                (mapc 
                 #'(lambda (str)
                     (format stream "~15T~A~%" str))
                 value))

               (else
                (format stream "~%~A: ~15T"
                        (capitalize slot))
                (my-pretty-print value stream)))))
   (all-the-slots)
   ))
    
(defmethod input-prototype ((self vp) slot-id)
  (case slot-id
    ((side)          #'(LAMBDA () (prompted-defined-symbol-input "Side   (pro/con): "
                                                                 `(pro con))))
    ((importance)    #'(LAMBDA () (prompted-defined-symbol-input "Importance (A-D): "
                                                                 `(a b c d))))
    ((type)          #'(LAMBDA () (prompted-defined-symbol-input "Type: (person/issue/record): "
                                                                 `(person issue record))))
    ((index-key)     #'(LAMBDA () (format nil "~A-~A-~A"
                                          (vp-type self)
                                          (vp-side self)
                                          (vp-importance self))))
    ((phrase)        #'(LAMBDA () (prompted-string-input "Phrase: ")))
    ((infinitive)    #'(LAMBDA () (prompted-string-input "Infinitive: ")))
    ((tense)         #'(LAMBDA () (prompted-defined-symbol-input "Tense: "
                                                                 `(present past perf plup
                                                                           ppart future futperf
                                                                           infinitive))))
    (otherwise       (call-next-method))))


(defmethod update-prototype ((self vp) slot-id)
  (case slot-id
    ((passive)       #'(LAMBDA () (prompted-symbol-input "Passive flag (T/nil): ")))
    ((no-negate)     #'(LAMBDA () (prompted-symbol-input "No-negate flag (T/nil): ")))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (vp-notes self))))
    ((side)          #'(LAMBDA () (PROG1 (prompted-defined-symbol-input "Side   (pro/con): "
                                                                        `(pro con))
                                    (sort-key self))))
    ((importance)    #'(LAMBDA () (PROG1 (prompted-defined-symbol-input "Importance (A-D): "
                                                                        `(a b c d))
                                    (sort-key self))))
    ((type)          #'(LAMBDA () (PROG1 
                                    (prompted-defined-symbol-input "Type: (person/issue/record): "
                                                                   `(person issue record))
                                    (sort-key self))))
    ((phrase)        #'(LAMBDA () (prompted-string-input "Phrase: ")))
    ((infinitive)    #'(LAMBDA () (prompted-string-input "Infinitive: ")))
    ((tense)         #'(LAMBDA () (prompted-defined-symbol-input "Tense: "
                                                                 `(present past perf plup
                                                                           ppart future futperf
                                                                           infinitive))))
    ((pronouns)      #'(LAMBDA () (prompted-list-input "Pronouns: ")))
    (otherwise       (call-next-method))))

(defmethod table-index-fields ((self vp))
    '(id index-key))


;;----------------------------------------------------------
;;      Master Data Base of Vps
;;----------------------------------------------------------

(setf vp (make-db)) 
(setf (db-name vp) 'vp) 
(setf (db-prompt vp) "VP> ")
(setf (db-class vp) 'vp)
(setf (db-commands vp)
      (append 
       '((notes (notes note))
         (type (type))
         (side (side))
         (importance (importance))
         (phrase (phrase))
         (no-negate (no-negate))
         (passive (passive))
         (infinitive (infinitive verb))
         (tense (tense))
         (pronouns (pronouns)))
       *record-db-commands*))

(init-db vp)



(defun prompted-defined-symbol-input (prompt choices)  
  (let ((input (prompted-symbol-input prompt)))
    (cond ((member input choices)
           input)
          (else
           (format t "~%Legal choices are: ~A~%" choices)
           (prompted-defined-symbol-input prompt choices)))))
