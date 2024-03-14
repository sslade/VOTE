;; loaded by load_utils.lisp

(defvar *db-files*)
(setf *db-files*
      '("specials_db" 
        "db" 
        "copy" 
        "db_cmds"
        "db_cmds2" 
        "edit" 
        "note"))

(defvar *dbs-files*)
(setf *dbs-files*
      '("record" 
        ;"task"
	))

; (my-load db "sched")
; (my-load db "pers")
; (my-load db "journal")
; (my-load db "areacode")

(my-load-files db *db-files*)
(my-load-files dbs *dbs-files*)

;** 9/9/16 moved from dbs/record.lisp
; (defpredicate record)
; (define-predicate record)
;** 9/9/16 moved from dbs/task.lisp
; (defpredicate task)
; (define-predicate task)

;; if arg is name of a database, that one is reloaded if already loaded.


(defun load-dbs (dbs &rest arg)  
  (if (not (boundp 'date-table))
      (init-date-table))
  (mapc 
   #'(lambda (db)
     (cond ((or (member db arg)
                (null (db-all db)))
            (setf *current-db* db)
            (get_command))
           (else
            (format t "~%Database already loaded: ~A" db)
            nil)))
   dbs)
  (values))
