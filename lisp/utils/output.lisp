
;;  (my-format t "In routine foo with args: ~A~%" args)
;;  (setf (my-format) nil)     --- turns off printing

(defun my-format (stream &rest args)
  (if (get 'my-format '*no-output*)
    (progn 
      (apply #'format stream args))
    (values)))
      
(defun (setf my-format) (val)
  (setf (get 'my-format '*no-output*) (null val)))


(defvar *no-output* nil)

;;  switches printing on or off...
;-----------------------------------------------------------------
(defmacro  out-switch (&rest body)
 `(let ((code #'(lambda () ,@body)))
    (if (null *no-output*)
        (funcall code)
        (values))))

;; 11/24/2015
(defun out-switch (n) (eval n))

;; (no-output t)    ---> turns off printing
;; (no-output nil)  ---> turns on printing
(defun no-output (flag)  
    (setf *no-output* flag)
    (setf (get 'my-format '*no-output*) (null flag))
    flag)
  
