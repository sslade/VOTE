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
     (SBCL "/c/cs458/lisp/")
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


(my-load vote-root "load_utils")

(defpredicate stance)
(defpredicate issue)
(defpredicate relation)
(defpredicate member)
(defpredicate group)
(defpredicate bill)
(defpredicate record)
(defpredicate db)


(format t "The code is loaded.  Use (init-pol) to load the databases.~%")

