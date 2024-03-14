

;; verify accuracy of type relations in database


;; source-db second-accessor accessor target-db

(defvar *type-relations*   
  `((,issue () ,#'issue-groups ,group)
    (,issue () ,#'issue-isa   ,issue)
    (,group () ,#'group-isa   ,group)
    (,group () ,#'group-issues ,issue)
    (,group ,#'stance-issue ,#'group-stances ,issue)
    (,member () ,#'member-groups ,group)
    (,member () ,#'member-committees ,group)
    (,member ,#'stance-issue ,#'member-credo ,issue)
    (,member ,#'relation-group ,#'member-relations ,group)
    ))


(defun check-type-rels (args)  
  (format t "~%Checking: ~A" args)
  (let ((source-db (first args))
        (second-accessor (second args))
        (first-accessor (third args))
        (target-db (fourth args)))
    (mapc 
     #'(lambda (item)
         (mapc 
          #'(lambda (item2)
              (let ((target (if second-accessor
                              (funcall second-accessor item2)
                              item2)))
                (if target
                  (get-node target target-db)))
              )
          (funcall first-accessor item))
         )
     (db-all source-db))))


(defun check-all ()
  (mapc #'check-type-rels *type-relations*))
