
(defun copy_command (&rest args)  
  (declare (ignore args))
  (let* ((items (db-current-items *current-db*))
         (new-items 
          (mapcar #'(lambda (item)
                 (let ((new-item (copy-instance item)))
                   (setf (slot-value new-item 'symbol) (generate-symbol (type-of new-item)))
                   (insert-item *current-db* new-item)
                   (push-update new-item 'copy_command *current-db*)
                   new-item))
               items)))
    (sort-db *current-db*)
    (mapc #'(lambda (item)
            (print-header item *standard-output*))
          new-items)))



(defun copy_undo (&rest args)  
  (declare (ignore args))
  (let ((items (db-current-items *current-db*)))
    #'(lambda ()
      (mapc
       #'(lambda (item)
           (declare (ignore item))
           (pop-update *current-db*)
           (format t "~%Removing copied item from update list.~%"))
       items))))


;; T's version of gensym permits symbol arguments
(defun generate-symbol (sym)
  (let ((str (->string sym)))
    (gensym str)))

      
 
