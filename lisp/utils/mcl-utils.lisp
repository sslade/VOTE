;; implementation specific functions
;; MCL versions
(in-package common-lisp-user)

;;
(defun hpos (stream)
  (stream-column stream))

;; See 
;; returns 2 values ...
(defun line-length (stream)
  (stream-line-length stream)
  )


(my-load utils "snd")

(play_vote_sounds 128)


(play_vote_sounds 129)
