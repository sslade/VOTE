
(require :resources)

;(with-open-resource-file (f (choose-file-dialog))
;  (let ((soundressorcehandle
;         (#_GetResource "snd " 128)))
;    (require-trap #_sndplay (%null-ptr )
;                  soundressorcehandle 0)))


;; Note: num = 128 for "Welcome to VOTE" message
;;       num = 129 for Chewbacca groan
;;       num = 130 for "Loading is complete."
;;       num = 27958 for "It's decision time."
;; ==============================================
(defun play_vote_sounds (num)
  (if (eq *implementation-type* 'mcl)
    (ccl::with-open-resource-file 
      (f (open "vote:utils;vote_sounds" :direction :input))
      (let ((soundressorcehandle
             (#_GetResource "snd " num)))
        (require-trap #_sndplay (%null-ptr )
                      soundressorcehandle 0)))))

