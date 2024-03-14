
;;----------------------------------------------------------
;;      Edit
;;----------------------------------------------------------

(defun edit_command (&rest args)  
  (let ((slot (car args)))
    (cond (slot (edit-field slot (cdr args)))
          (else
           (format t "Usage: edit <fieldname>~%")))))


(defun edit-field (slot args)
  (declare (ignore args))
  (let* ((items (db-current-items *current-db*))
         (proc (update-prototype (car items) slot)))
    (mapc #'(lambda (item)
              (let ((old_val (slot-value item slot)))
                (cond ((null old_val)
                       (format t "~%~A field is empty.  Nothing to edit."
                               slot))
                      ((not (consp old_val))
                       (format t "~%~A field is not a list.  Simply replace with command: ~A."
                               slot slot))
                      (else
                       (push-update item 'slot *current-db*)
                       (setf (slot-value item slot)
                             (edit-item old_val nil proc))
                       (print-header item *standard-output*)))))
            items)))


(defun edit-item (old-val new-val proc)  
  (cond ((null old-val) new-val)
        (else
         (case (prompt-edit-item (car old-val))
           ((delete)  (edit-item (cdr old-val) new-val proc))
           ((replace) (edit-item (cdr old-val) (edit-combine new-val (funcall proc)) proc))
           ((insert)  (edit-item old-val (edit-combine new-val (funcall proc)) proc))
           ((OK)      (edit-item (cdr old-val) (edit-combine new-val (car old-val)) proc))
           ((quit)    (edit-combine new-val old-val))
           (otherwise nil)))))

(defun prompt-edit-item (value)  
  (format t "~%~A : ~A"
          value
          "Edit? ")
  (let ((input (lread)))
    (case (if input
              (char input 0)
              #\return)                
      ((#\D #\d) 'delete)
      ((#\Y #\y #\R #\r) 'replace)
      ((#\I #\i) 'insert)
      ((#\Q #\q) 'quit)
      ((#\N #\n #\return) 'OK)
      (otherwise
       (format t "~%Y -- yes, edit
N -- no, do not edit
D -- delete
I -- insert new stuff before this item
R -- replace (same as edit)
Q -- quit
CR -- OK (same as do not edit)")
       (prompt-edit-item value)))))
            

(defun edit-combine (&rest l)  
  (flatten (apply #'list l)))

;; flatten defined in new_utils
(defun flatten2 (l)  
  (cond ((null l) nil)
        ((consp (car l))
         (append (flatten (car l)) (flatten (cdr l))))
        ((null (car l))
         (flatten (cdr l)))
        (else
         (cons (car l) (flatten (cdr l))))))


(defun edit_undo (&rest args) 
  (declare (ignore args))
  #'(lambda (&rest args)
    (undo-field (car args) (cdr args))))

