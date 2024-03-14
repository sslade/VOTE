
;; (require (db load_db))

(defvar *pol-files*)
(setf *pol-files*
      '("specials_pol"
;; utilities et alia
        "utils" 
        "isa" 
        "sort"
        "mypp" 
        "operations"
        "remove" 
;; data base definitions
        "group"
        "issue" 
        "bill"
        "member" 
        "strategy"
;; links among data base items
        "stance" 
        "relation"
        "decision" 
;; top level
        "cg" 
;; decision strategies code
        "strats" 
        "party-strat" 
        "shift" 
        "protocol"
;; analysis code
        "mem_anal"
        "analyze" 
        "anal2"
        "anal3"
;; data dependency 
        "dd"
;; parsing bill-remarks
        "parse"
;; new
       "types"
	))

;; (my-load pol "district")  *??

(my-load-files pol *pol-files*)


;; test modules
;; ***** (my-load test "load_test")
;;------------------------------------------------------------------
;;  init-pol  -- initialize the pol dbases
;;------------------------------------------------------------------


;;  (init-pol member)  --> reloads member db
   

(defun init-pol (&rest arg)  
  (newload-dbs
   (list
    group
    bill
    issue
 ;;   district
    member
    strategy)
   )
)
  ;; 11/11/15 
 ;; 11/24/15 (load "loaddata.lisp"))

;; 11/28/2019
(defun newload-dbs (dbs &rest arg)  
  (format t "~%newload-dbs\n")
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
  )
