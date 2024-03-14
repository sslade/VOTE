;;  Enhanced tables
;;          (my-table-walk self proc)
;;          (my-table-entry self key)
;;          (my-table? self)
;;          (my-make-table id)
;;          (table-intern table keys value &optional stop-table)
;;          (table-unintern table keys value &optional stop-table)
;;          (my-table-intern table keys value &optional stop-table)
;;          (my-table-unintern table keys value &optional stop-table)

(defstruct (my-table
            (:predicate my-table?)
            (:print-function
             (lambda (p s k)
               (format s "#{My-table (~A) ~A}"
                       (object-hash p)
                       (name p))))
			(:conc-name nil))
  (table (make-hash-table)
         :type hash-table)
  (name nil
        :type symbol)
  )

;; 11/5/2015 
;;(defmethod make-load-form ((tab my-table))
;;  (make-load-form-saving-slots tab '(table name)))

(defmethod my-table-walk ((self my-table) (proc function))
  (maphash proc (table self))
  (values))

(defmethod my-table-entry ((self my-table) key)
  (gethash key (table self)))

(defmethod my-table-set ((self my-table) key value)
  (setf (gethash key (table self)) value))


;;  create an instance of my-table
;;  which puts a hash-table in the table slot
;;  put the id in the name slot
;;  define the id to be an access function for the table
;;  define the id to have a setf function for the table
;;  ----------------------------------------------------


(defmacro my-make-table (id)
  `(progn
     (defvar ,id (make-my-table))
     (setf (name ,id) ',id)
     (defun ,id (key) (gethash key (table ,id)))
     (defun (setf ,id)  (value key)
       (setf (gethash key (table ,id)) value))
     ,id))


(defmethod id ((self my-table)) (name self))

(defmethod pretty-print ((self my-table) stream)
  (my-table-walk
   self
   #'(lambda (key value)
	   (format stream "~% ~A ~20T ~A" key value)))
  )



;; table-intern works with regular hash tables, not my-tables...
(defun table-intern (table keys value &optional stop-table)
  (cond ((null keys) nil)
        ((not (hash-table-p table))
         'bad-table-in-table-intern)
        ((stringp keys)
         (table-intern table (read-string->list keys) value stop-table))
        ((symbolp keys)
         (if (and stop-table (gethash keys stop-table))
           nil
           (setf (gethash keys table) (adjoin value (gethash keys table)))))
        ((listp keys)
         (table-intern table (car keys) value stop-table)
         (table-intern table (cdr keys) value stop-table))
        (else 'error-in-table-intern)))

(defun table-unintern (table keys value &optional stop-table)
  (cond ((null keys) nil)
        ((not (hash-table-p table))
         'bad-table-in-table-unintern)
        ((stringp keys)
         (table-unintern table (read-string->list keys) value stop-table))
        ((symbolp keys)
         (if (and stop-table (gethash keys stop-table))
           nil
           (setf (gethash keys table) (delete value (gethash keys table)))))
        ((listp keys)
         (table-unintern table (car keys) value stop-table)
         (table-unintern table (cdr keys) value stop-table))
        (else 'error-in-table-unintern)))


(defun my-table-intern (table keys value &optional stop-table)
  (if (my-table? stop-table) (setq stop-table (table stop-table)))
  (table-intern (table table) keys value stop-table))

(defun my-table-unintern (table keys value &optional stop-table)
  (if (my-table? stop-table) (setq stop-table (table stop-table)))
  (table-unintern (table table) keys value stop-table))

