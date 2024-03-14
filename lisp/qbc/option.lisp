
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
(defvar option)

(defclass option (record)
  ((name     :initform nil :documentation "name of option" :accessor option-name) 
   (english  :initform nil :documentation "English version of option name" :accessor option-english)
   (sort-key :initform nil :documentation "option Sort Key" :accessor option-sort-key)
   (synonyms :initform nil :documentation "option synonyms - list of symbols" :accessor option-synonyms)
   (isa      :initform nil :documentation "List of symbols" :accessor option-isa)
   (isa-depth :initform nil :documentation "String for padding printing" :accessor option-isa-depth)
   (instances :initform nil :documentation "List of options" :accessor option-instances)
   (importance :initform nil :documentation "Intrinsic importance of option" :accessor option-importance)
   (data :initform nil :documentation "Data for option" 
         :accessor option-data)
   (notes     :initform nil :documentation "List of remarks" :accessor option-notes))
  (:documentation "option Class"))

(defpredicate option)

(defmethod all-the-slots ((obj option))
  (append '(name english sort-key synonyms isa isa-depth instances importance
            data notes)
          (call-next-method)))


(defmethod print-object ((self option) stream)
  (format stream "\\verb|#{Option (~A) ~A}|"
          (object-hash self) 
          (option-name self)))
   
(defmethod english ((self option))
  (cond ((option-english self))
        (t
         (concatenate 'string "**the " (option-name self) "**"))))

(defmethod english-short ((self option))
  (option-name self))

(defmethod inorder? ((self option) (other option))
  (if (option-p other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (cond ((and (date? first) (date? second))
             (inorder? first second))
            (t
             (string< first second))))
    nil))

(defmethod synonyms ((self option))
  (let ((name (read-string->list (option-name self))))
    (if (= (length name) 1)
      (append name (option-synonyms self))
      (option-synonyms self))))

(defmethod sort-key ((self option))
  (if (option-sort-key self)
    (option-sort-key self)
    (option-name self)))

(defmethod set-isa-sort ((self option))
  (setf (option-isa-depth self) (isa-depth self option))
  (setf (option-sort-key self) (generate-isa-sort-key self option)))

(defmethod set-alpha-sort ((self option))
  (setf (option-isa-depth self) "")
  (setf (option-sort-key self) (option-name self)))

(defmethod id ((self option))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'option)))
        (t (record-symbol self))))
   
(defmethod print-readable2 ((self option) stream)
  (format stream "~%(get-node ~A option)"
          (id self)))

(defmethod print-header ((self option) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~18T[~A]~28T ~A~50T~A"
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
          (option-importance self)
          (option-name self)
          (cond ((option-data self))
                (t ""))))


(defmethod print-hc ((self option) stream &rest args)
  (declare (ignore args))
  (format stream "\\hinge{}~%[~A] ~15T{\\rm ~A}~%"
          (option-importance self)
          (option-name self))
  (mapc
   #'(lambda (sel-id)
       (let ((selector (car sel-id))
             (identifier (cdr sel-id)))
         (if (funcall selector self)
           (progn 
             (format stream "~%\\H ~A~26T"
                     identifier)
             (pretty-print (funcall selector self) stream)
             ))))
   (list
    (cons #'option-data "Data:")
    ))
  
  )


(defmethod pretty-print ((self option) stream)
  (call-next-method)
  (cond ((option-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (option-notes self))))
  )


(defmethod input-prototype ((self option) slot)
  (case slot
    ((isa-depth)     #'(LAMBDA () ""))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((data)          #'(LAMBDA () (prompted-datum-list-input "Data: " (id self) option)))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    (otherwise       (call-next-method))))



(defmethod update-prototype ((self option) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((data)          #'(LAMBDA () (prompted-datum-list-input "Data: " (id self) option)))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (option-notes self))))
    (otherwise       (call-next-method))))


(defmethod no-print-readable-fields ((self option))
  (append '(sort-key instances isa-depth) (call-next-method)))

(defmethod table-index-fields ((self option))
  (append '(synonyms name keywords isa) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of options
;;----------------------------------------------------------

;; option is not defvar'd in db.lisp
(defvar option)
(setq option (make-db)) 
(setf (db-name option) 'option) 
(setf (db-prompt option) "option> ")
(setf (db-class option) 'option)
(setf (db-commands option)
      (append
       '((notes (notes note))
         (name (name))
         (isa (isa))
         (english (english))
         (synonyms (synonyms synonym names))
         (data (data))
         )
       *record-db-commands*))

(init-db option)


