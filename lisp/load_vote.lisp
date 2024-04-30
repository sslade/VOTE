;;;;    -------------------------------------------------------------
;;;;
;;;;    Table of Contents:
;;;;
;;;;    1. Turn on verbose feedback during load.
;;;;    3. Determine implementation type (MCL or Allegro)   
;;;;    4. Definition of ELSE constant   
;;;;    5. Define VOTE-ROOT directory for VOTE code.   **** specified by user ****
;;;;    6. Loads file containing definition of MY-LOAD (smart loader)   
;;;;    7. Defines portable pathnames 
;;;;    8. Loads all the standard stuff
;;;;    
;;;;    -------------------------------------------------------------
(setq *load-verbose* t)


(defvar *implementation-type* 
  (let ((implementation (lisp-implementation-type)))
    (cond ((string-equal implementation "Macintosh Common Lisp") 'MCL)
          ((string-equal implementation "Allegro CL") 'Allegro)
          ((string-equal implementation "SBCL") 'SBCL)
          (t 'unknown))))


;; PowerPC version of MCL uses compiler extension "pfsl"
(defvar *compiled-extension*
  (let ((version (lisp-implementation-version)))
    (cond ((and (eq *implementation-type* 'MCL)
                (string-equal version "Version 4.0"))
           "pfsl")
          (t "fasl"))))


;; 11/5/2015
;; (defconstant else t "Used for defaults in COND statements")

; **************************************************************
;    Edit the following pathnames to correspond to your version
;    of LISP and the VOTE home directory.  Be sure to end the path
;    with the appropriate separator, e.g., : or /
; **************************************************************
(defconstant VOTE-ROOT
  (pathname
   (case *implementation-type*
     (MCL "Macintosh HD:lisp:")
     (Allegro "/class/sslade/vote/")
     (SBCL "/Users/gnn/Repos/Yale/VOTE/lisp/")
     (otherwise nil)))
  "Root directory in which VOTE code is installed")

;; MCL logical pathname.
(if (eq *implementation-type* 'mcl)
	(setf (logical-pathname-translations "vote")
		  `(("**;*.*" ,(merge-pathnames ":**:*.*" vote-root)))))

     
; **************************************************************
;;; *** must compile myload file first ***
;;; *** see readme file ***
; **************************************************************
(load (merge-pathnames (format nil "myload.~A" *compiled-extension*) vote-root))

(defun name-directory (root sub)
   (let ((separator
          (case *implementation-type*
            (MCL ":")
            (Allegro "/")
            (SBCL "/"))))
     (pathname (format nil "~A~A~A" (namestring root) sub separator))))


;; default pathnames for use by my-load
;; ---------------------------------------------------------------

(defvar utils (name-directory vote-root "utils"))
(defvar db    (name-directory vote-root "db"))
(defvar dbs   (name-directory db "dbs"))
(defvar data  (name-directory db "data"))
(defvar pol   (name-directory vote-root "pol"))
(defvar gen   (name-directory vote-root "gen"))
(defvar test  (name-directory vote-root "test"))


;; implementation specific stuff
(case *implementation-type*
  (MCL (my-load utils "mcl-utils"))
  (Allegro (my-load utils "allegro"))
  (SBCL (my-load utils "sbcl"))
  (otherwise nil))


;; (my-load vote-root "load_utils")
(defvar *english* T)
(defvar *french* T)
(defvar *japanese* T)

(my-load utils   "generics" )
(my-load utils   "copy" )
;;    "ctime" 
(my-load utils   "io" )
(my-load utils   "ionew" )
(my-load utils   "macros" )
(my-load utils   "object_hash" )
(my-load utils   "props" )
(my-load utils   "new_utils" )
(my-load utils   "spell" )
(my-load utils   "output" )
(my-load utils   "run-time" ) 
(my-load utils   "date")
(my-load utils   "tables" )


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

;; gen/load_db.lisp

(my-load db "specials_db" )
(my-load db   "db" )
(my-load db   "copy" )
(my-load db   "db_cmds")
(my-load db   "db_cmds2") 
(my-load db   "edit" )
(my-load db   "note")


(my-load dbs   "record")

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

;; (my-load gen "load_gen")

(my-load gen "specials_gen")
(my-load gen   "clusters")
(my-load gen   "context" )
(my-load gen   "english" )
(my-load gen   "gen_utils") 
(my-load gen   "bill-english")
(my-load gen   "nouns")
(my-load gen   "np" )
(my-load gen   "generics")
(my-load gen   "preamble" )
(my-load gen   "rels")
(my-load gen   "verbs" )

(my-load gen   "combine" )
(my-load gen   "downside" )
(my-load gen   "vp" )
(my-load gen   "test")
(my-load gen   "explain" ) 

;; load pol files
;; (my-load pol "load_pol")

(my-load pol "specials_pol")
;; utilities et alia
(my-load pol    "utils" )
(my-load pol    "isa" )
(my-load pol    "sort")
(my-load pol    "mypp" )
(my-load pol    "operations")
(my-load pol    "remove" )
;; data base definitions
(my-load pol    "group")
(my-load pol    "issue" )
(my-load pol    "bill")
(my-load pol    "member" )
(my-load pol    "strategy")
;; links among data base items
(my-load pol    "stance" )
(my-load pol    "relation")
(my-load pol    "decision" )
;; top level
(my-load pol    "cg" )
;; decision strategies code
(my-load pol    "strats" )
(my-load pol    "party-strat" )
(my-load pol    "shift" )
(my-load pol    "protocol")
;; analysis code
(my-load pol    "mem_anal")
(my-load pol    "analyze" )
(my-load pol    "anal2")
(my-load pol    "anal3")
;; data dependency 
(my-load pol    "dd")
;; parsing bill-remarks
(my-load pol    "parse")
;; new
(my-load pol   "types")




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


(defpredicate stance)
(defpredicate issue)
(defpredicate relation)
(defpredicate member)
(defpredicate group)
(defpredicate bill)
(defpredicate record)
(defpredicate db)

(define-predicate stance)
(define-predicate issue)
(define-predicate relation)
(define-predicate member)
(define-predicate group)
(define-predicate bill)
(define-predicate record)
(define-predicate db)
(define-predicate date)
(define-predicate strategy)



(format t "The code is loaded.  Use (init-pol) to load the databases.~%")

