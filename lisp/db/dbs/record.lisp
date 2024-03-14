;; record class for database


(defclass record ()
  ((index        :initform nil :type integer :accessor index)
   (status       :initform "Active" :type string :accessor status)
   (date-open    :initform nil :accessor date-open)
   (date-closed  :initform nil :accessor date-closed)
   (current-flag :initform nil :type symbol :accessor current-flag)
   (symbol       :initform nil :documentation "unique atomic identifier" :accessor record-symbol)
   (subject      :initform nil :documentation "text header" :accessor subject :type string)
   (keywords     :initform nil :accessor keywords)
   (remarks      :initform nil :documentation "textual comments" :accessor remarks)
   (db           :initform nil :accessor db :accessor item-db))
  (:documentation "Generic record for database"))

; (defpredicate record)


(defmethod id ((obj record))
  (record-symbol obj))

(defmethod input-prototype ((obj record) slot)
  (case slot
    ((symbol)    #'(LAMBDA () (generate-symbol (class-name (class-of obj)))))
    ((subject)   #'(LAMBDA () (prompted-string-input "Subject: ")))
    ((status)    #'(lambda () "Active"))
    ((date-open) #'(lambda () (current_date)))
    (otherwise   #'(lambda () nil))))

(defmethod update-prototype ((obj record) slot)
  (case slot
    ((status)      #'(lambda () (capitalize (prompted-string-input "Status: "))))
    ((date-open)   #'(lambda () (prompted-date-input "Date-open: ")))
    ((date-closed) #'(lambda () (prompted-date-input "Date-closed: ")))
    ((subject)     #'(lambda () (prompted-string-input "Subject: ")))
    ((remarks)     #'(lambda () (prompted-string-list-input "Remarks: ")))
    ((keywords)    #'(lambda () (prompted-list-input "Keywords: ")))
    ((db)          #'(lambda () (prompted-string-input "Db: ")))
    (otherwise     #'(lambda () nil))))


(defgeneric all-the-slots (class)
  (:method ((class record)) 
           '(index status date-open date-closed current-flag 
             symbol subject remarks keywords db)))


(defgeneric no-pp-fields (class)
  (:method ((class record)) '(index current-flag)))

(defgeneric no-print-readable-fields (class)
  (:method ((class record)) '(index current-flag)))

(defgeneric table-index-fields (class)
  (:method ((class record)) '(subject keywords symbol)))

;; make this a before method?
(defmethod print-header ((self record) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T"
          (case (char (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise  (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)))
 

(defmethod pretty-print ((self record) stream)
  (mapc 
   #'(lambda (slot)
       (cond ((null (slot-value self slot))
              nil)

             ((member slot (no-pp-fields self))
              nil)

             ((eq slot 'db)
              (format stream "~%Db: ~15T")
              (print-object (slot-value self slot) stream))

             ((eq slot 'remarks)
              (format stream "~%Remarks:")
              (mapc 
               #'(lambda (str)
                   (format stream "~%~A" str))
               (slot-value self slot)))

             (else
              (format stream "~%~A: ~15T"
                      (capitalize slot))
              (pretty-print (slot-value self slot) stream))))
   (all-the-slots self))
   (values))


(defmethod print-readable ((self record) stream)
  (format stream "~%(defvar ~A)~%(prog1 (setf ~A (make-instance '~A))"
          (id self) (id self) (type-of self))
  (mapc 
   #'(lambda (slot)
       (cond ((null (slot-value self slot))
              nil)
             ((member slot (no-print-readable-fields self))
              nil)
;; *** figure out what to do here ***
      ;;       ((eq slot 'db)
      ;;        (format stream "~%Db: ~15T")
      ;;        (print-object (slot-value self slot) stream))

;             ((eq slot 'remarks)
;              (format stream "~%Remarks:")
;              (mapc 
;               #'(lambda (str)
;                   (format stream "~%~A" str))
;               (slot-value self slot)))

             (else
              (format stream "~%(setf (slot-value ~A '~A) "
                      (id self) slot)
              (print-readable (slot-value self slot) stream)
              (format stream ") "))))
   (all-the-slots self))
  (format stream ")")
  (values))


(defvar *record-db-commands*
      '((date-closed (date-closed closed))
        (date-open (date-open open))
        (remarks (remarks text))         
        (status (status st))
        (subject (subject subj))    
        (keywords (keywords keys))
        (db (db))  ))


;; various predicates on the status of a record
;;---------------------------------------------
(defmethod open? ((self record))
  (not (closed? self)))

(defmethod closed? ((self record))
  (string= (status self) "Completed"))

(defmethod pending? ((self record))
  (string= (status self) "Pending"))

(defmethod active? ((self record))
  (string= (status self) "Active"))

(defmethod deleted? ((self record))
  (string= (status self) "Deleted"))

(defmethod copy ((self record))
  (copy-instance self))
