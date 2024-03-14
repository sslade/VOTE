(in-package common-lisp-user)

;; default language for generation is English
(defvar *english* T)

;; Files to load.
;; ---------------------------------------------------------------

(defvar *utils-files*)
(setf *utils-files*
      '("generics" 
        "copy" 
;;        "ctime" 
        "io" 
        "ionew" 
        "macros" 
        "object_hash" 
        "props" 
        "new_utils"
        "spell" 
        "output" 
        "run-time" 
;;        "date"
        "tables"

	))

;; (define-predicate ctime)
;;(defpredicate ctime)

(defun my-load-files (dir files)
  (mapc #'(lambda (file) (my-load dir file)) files))

(my-load-files utils *utils-files*)



;; (define-predicate date)
;; (defpredicate date)

(defvar bill)
(defvar issue)
(defvar group)
(defvar member)
(defvar strategy)
(defvar record)
(defvar *db-files*)
(defvar *dbs-files*)
(defvar *gen-files*)
(defvar *pol-files*)



;; load database files
(my-load db "load_db")

;** 9/9/16 moved from db/db.lisp
;;(make-executable *db-table*)

;; language modules
;;(if *english*
(my-load gen "load_gen")
;;    (my-load french "load_gen"))


;; load pol files

(my-load pol "load_pol")
;; also loads generation files

;** 9/9/16 moved from pol/*
;; (defpredicate group) => (define-predicate group)
(define-predicate group) 
(define-predicate bill)
(define-predicate issue)
(define-predicate member)
(define-predicate strategy)
(define-predicate stance)
(define-predicate relation)
(define-predicate decision)
(define-predicate np)
(define-predicate vp)

;; turn off debugging messages
(setf db-format nil)


;; batch compilation functions
;; ---------------------------
(defun my-compile-files (dir files)
  (mapc 
   #'(lambda (file) (compile-file (merge-pathnames dir file) :verbose T)) 
   files))

(defun compile-everything ()
  (my-compile-files utils *utils-files*)
  (my-compile-files db *db-files*)
  (my-compile-files dbs *dbs-files*)
  (my-compile-files pol *pol-files*)
  (my-compile-files gen *gen-files*)
  )

(defun compile-dbs ()
  (mapc
   #'(lambda (db)
	   (compile-db db))
   (list bill issue group member strategy)))

(defvar *current-db*)

(defun compile-db (db)
  (setf *current-db* db)
  (get_command)
  (compile_command))

(format t "~%End of load_utils.lisp~%")
