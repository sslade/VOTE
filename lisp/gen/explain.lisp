
(defun explain-to-audience (dec-id group-id)  
  (let* ((grp (get-node group-id group))
         (new-dec (copy dec-id))
         (grp-stances (group-stances grp))
         (reasons (flatten (decision-reason new-dec)))
         (downside (flatten (decision-downside new-dec)))
         (down-rec (flatten (decision-downside-record new-dec)))
         (proc #'(lambda (st)
                 (member st grp-stances :test #'stance-alikev?))))
    (print new-dec *standard-output*)

    (setf (decision-reason new-dec)
         (group-reasons (collect reasons proc)))
    (setf (decision-downside new-dec)
         (group-reasons (collect downside proc)))
    (setf (decision-downside-record new-dec)
         (collect down-rec proc))

    (english-rationale new-dec *standard-output*)

    new-dec))


; (function-synonym eta explain-to-audience) 

(defun sarcastic-explanation (dec-id)  
  (let* ((new-dec (copy dec-id))
         (reasons (mapcar #'copy (flatten (decision-reason new-dec))))
         (downside (mapcar #'copy (flatten (decision-downside new-dec))))
         (down-rec (mapcar #'copy (flatten (decision-downside-record new-dec)))))
    (flet ((proc (st)
             (if (stance-p st)
               (let ((side (stance-side st)))
                 (setf (stance-side st)
                       (flip-side side))
                 st))))
      (setf (sarcasm?) t)
      (print new-dec *standard-output*)

      (setf (decision-reason new-dec)
            (group-reasons (mapcar #'proc reasons)))
      (setf (decision-downside new-dec)
            (group-reasons (mapcar #'proc downside)))
      (setf (decision-downside-record new-dec)
            (mapcar #'proc down-rec))

      (english-rationale new-dec *standard-output*)
      (setf (sarcasm?) nil)
      new-dec)))

(defun flip-side (side)  
  (case side
    ((pro) 'con)
    ((con) 'pro)
    (otherwise nil)))

;; (function-synonym se sarcastic-explanation) 
(setf (symbol-function 'se) (symbol-function 'sarcastic-explanation))

;; simple toggle for turning on and off sarcasm.
(let ((sarcasm-flag nil))
  (defun sarcasm? () sarcasm-flag)
  (defun (setf sarcasm?) (val) (setf sarcasm-flag val)))




