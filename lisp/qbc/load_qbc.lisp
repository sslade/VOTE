;; This is the main load file for VOTE.
;; need to have another pathname for Allegro
(load "Macintosh HD:lisp:load_vote.lisp")

;; *** new qbc stuff
(setf qbc   (name-directory vote-root "qbc"))
(setf data  (name-directory qbc "data"))

;; *** new qbc stuff
(my-load qbc "qbc")

(my-load qbc "agent")
(my-load qbc "preference")
(my-load qbc "option")
(my-load qbc "choose")
(my-load qbc "datum")
(my-load qbc "choice")
;; (my-load qbc "x")

(defvar *qbc-dbs*)
(setf *qbc-dbs* (list quantity option agent))

(defun init-qbc (&rest arg)  
  (load-dbs
   *qbc-dbs*
   (car arg)))

(format t "~%The qpc code is loaded.  Use (init-qbc) to load the databases.~%")

(init-qbc)

