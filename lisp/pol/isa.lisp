
(defun get-isa (node db)  
  (let ((isa-list (isa node)))
    (cond ((null isa-list) nil)
          (else
           (get-node (car isa-list) db)))))

(defun get-node (name db &rest no-print)  
  (cond ((null (db-all db))
         (format t "~%Problem in GET-NODE.  The ~A database is not loaded.~%"
                 db)
         nil)
        (else
         (let* ((class (db-class db))
                (table (db-table db)))
           (cond ((typep name class) name)
                 ((gethash name table)
                  (get-node-from-list (gethash name table) name))
                 (else
                  (let ((word (spell-match table name)))
                    (cond (word
                           (format t "~%Spelling correction for ~A data base: ~A ==> ~A~%"
                                   (db-name db) name word)
                           (get-node-from-list (gethash word table) word))
                          ;; don't print message if no-print flag is given                 
                          ((car no-print)
                           nil)
                          (else
                           (format t "No entry for ~A in db: ~A (get-node)~%" name db)
                           nil)))))))))

(defun get-node-from-list (lst name)  
  (cond ((null lst) nil)
        ((null (cdr lst))
         (car lst))
        ((member name (synonyms (car lst)))
         (car lst))
        (else
         (get-node-from-list (cdr lst) name))))
  

(defun generate-isa-sort-key (node db)  
  (let ((isa (get-isa node db))
        (node-name (slot-value node 'name)))
    (cond ((null isa)
           node-name)
          (else
           (concatenate 'string (generate-isa-sort-key isa db) "-" node-name)))))

(defun isa-depth (node db)  
  (let ((isa-link (get-isa node db)))
    (cond ((null isa-link) "")
          (else
           (concatenate 'string "." (isa-depth isa-link db))))))

