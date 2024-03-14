;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
;;
;;  Notes:
;;
;;      Note structure type
;---------------------------------------------------

(defclass note ()
  ((symbol    :initform nil :documentation "Unique atomic identifier" :accessor note-symbol)
   (date-open :initform nil :documentation "Date opened" :accessor note-date-open)
   (subject   :initform nil :documentation "Text header string" :accessor note-subject)
   (remarks   :initform nil :documentation "Text strings"  :accessor note-remarks)) 
  (:documentation "Note class"))

(defmethod id ((self note))
  (note-symbol self))

(defmethod pretty-print ((self note) stream)
  (mapc
   #'(lambda (slot)
       (cond ((null (slot-value self slot))
              nil)
             
             ((eq slot 'remarks)
              (format stream "~%Remarks:")
              (mapc #'(lambda (str)
                        (format stream "~15T~A~%" str))
                    (slot-value self slot)))
             
             (else
              (format stream "~%~A: ~15T"
                      (capitalize slot))
              (pretty-print (slot-value self slot) stream))))
   '(subject remarks))
  (values))

(defmethod print-header ((self note) stream &rest args)
  (declare (ignore args))
  (format stream "~%~10T~A ~28T~A"
          (print-header (note-date-open self) nil)
          (note-subject self)))

(defmethod print-object ((self note) stream)
  (format stream "#{Note ~A}"
          (object-hash self)))

(defmethod input-prototype ((self note) slot)
  (case slot
    ((symbol)        #'(LAMBDA () (generate-symbol 'note)))
    ((subject)       #'(LAMBDA () (prompted-string-input "Subject: ")))
    ((date-open)     #'(LAMBDA () (current_date)))
    ((remarks)       #'(LAMBDA () (prompted-string-list-input "Remarks: ")))
    (else            #'(LAMBDA () '()))))

