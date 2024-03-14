
(defun sort_command (&rest args)  
;;  (format t "Sort args: ~A~%" args)
  (case (car args)
    ((alpha)  (mapc #'set-alpha-sort (db-all *current-db*)))
    ((date)   (mapc #'set-date-sort (db-all *current-db*)))
    ((isa)    (progn 
                (mapc #'(lambda (node) (invert-instances node *current-db*))
                      (db-all *current-db*))
                (mapc #'set-isa-sort
                      (db-all *current-db*))))
    (otherwise
     (if (car args)
         (let ((sort-proc (create-sort-key-proc *current-db* (car args))))
           (mapc sort-proc (db-all *current-db*)))
         nil)))
  (sort-db *current-db*))


(defun create-sort-key-proc (db slots)  
  #'(lambda (item)
      (setf (slot-value item 'sort-key)
            (apply #'string-append (mapcar #'(lambda (slot) (if (slot-value item slot)
                                                              (->string (slot-value item slot))
                                                              " "))
                                           slots)))))


(defun invert-instances (node db)  
  (let* ((isa-list (slot-value node 'isa)))
    (cond ((null node) nil)
          ((null isa-list) nil)
          (else
           (mapc 
            #'(lambda (isa)
                (let* ((isa-node (get-node isa db))
                       (instances (slot-value isa-node 'instances)))
                  (setf (slot-value isa-node 'instances)
                        (enter instances node))))
            isa-list)))))
