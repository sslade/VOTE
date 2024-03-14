

;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
;;
;;  Tasks:
;;
;;      Task structure type
;---------------------------------------------------


(defclass task (record)
  ((time     :initform nil :documentation "time of engagement" :accessor task-time)
   (date-due :initform nil :documentation "date due" :accessor date-due)
   (client   :initform nil :documentation "person requesting task" :accessor client)
   (agent    :initform nil :documentation "person performing task" :accessor agent))
  (:documentation "Task Class"))

; (defpredicate task)

(defmethod all-the-slots ((obj task))
  (append '(time date-due client agent)
          (call-next-method)))


(defmethod id ((obj task))
  (record-symbol obj))


(defmethod inorder? ((self task) other)
  (if (task-p other)
    (let ((date1 (if (date-due self)
                   (coerce (date-due self) 'date)
                   (today)))
          (date2 (if (date-due other)
                   (coerce (date-due other) 'date)
                   (today)))
          (time1 (if (task-time self) 
                   (coerce (task-time self) 'ctime)
                   (make-ctime)))
          (time2 (if (task-time other) 
                   (coerce (task-time other) 'ctime)
                   (make-ctime)))
          (status1 (status self))
          (status2 (status other)))
      (or (date-before? date1 date2)
          (and (date-equal? date1 date2)
               (or (time-before? time1 time2)
                   (and (time-equal? time1 time2) 
                        (inorder? status1 status2))))))))

;; replace call-next-method with :before method on record ???
;;;************************
(defmethod print-header ((self task) stream &rest args)
  (declare (ignore args))
  (call-next-method)
  (format stream "~A ~21T~A~28T~A~A"
          (if (date? (date-due self))
            (print-header (date-due self) nil)
            "")
          (if (ctime? (task-time self))
            (print-header (task-time self) nil)
            "")
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (subject self)))


(defmethod print-hc ((self task) stream &rest args)
  (let ((indent (cond ((stringp args) args)
                      ((and (listp args)
                            (stringp (car (flatten args))))
                       (car (flatten args)))
                      (else ""))))
 ;;   (db-format t "~%Args: ~A.  ~30TIndent: ~A." args indent)
    (format stream "~%~A ~A ~6T~A ~21T~A~28T~A~A~A"
            (case (char (status self) 0)
              ((#\A #\O) #\space)
              ((#\P) #\/)
              (otherwise  (char (status self) 0)))
;;;          (if (current-flag self) "*" " ")
            (if (db-p (db self)) "db" "  ")
;;;          (index self)
            (if (date? (date-due self))
              (print-header (date-due self) nil)
              "")
            (if (ctime? (task-time self))
              (print-header (task-time self) nil)
              "")
            indent
            (if (db-p (db self))
              (format nil "~S: " (db-name (db self)))
              "")
            (subject self))
    (if (db-p (db self))
      (mapc 
       #'(lambda (item)
           (print-hc item stream (concatenate 'string indent "  ")))
       (db-all (db self))))))

(defmethod print-object ((self task) stream)
  (format stream "#<Task ~A Status: ~A>"
          (object-hash self)
          (status self)))


(defmethod input-prototype ((self task) slot)
  (case slot
    ((date-due)      #'(lambda () (prompted-date-input "Date-due: ")))
    (otherwise       (call-next-method))))

(defmethod update-prototype ((self task) slot)
  (case slot
    ((resources)     #'(lambda () (prompted-string-input "Resources: ")))
    ((date-due)      #'(lambda () (prompted-date-input "Date-due: ")))
    ((time)          #'(lambda () (prompted-time-input "Time: ")))
    ((client)        #'(lambda () (prompted-string-input "Client: ")))
    ((agent)         #'(lambda () (prompted-string-input "Agent: ")))
    (otherwise       (call-next-method))))

(defmethod table-index-fields ((self task))
  (append '(agent client) (call-next-method)))


;;----------------------------------------------------------
;;      Master Schedule of Tasks
;;----------------------------------------------------------

;; task is defvar'd in db.lisp
(setq task (make-db)) 
(setf (db-name task) 'task) 
(setf (db-prompt task) "TASK> ")
(setf (db-class task) 'task)
(setf (db-commands task)
      (append 
       '((agent (agent))
         (client (client))
         (time (time))
         (date-due (date-due due)))
       *record-db-commands*))


(init-db task)

