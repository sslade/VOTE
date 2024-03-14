
;;  calculate wall-clock time for running some code
;-----------------------------------------------------------------

(defmacro run-time (&rest body)
 `(let ((code #'(lambda () ,@body))
        (start (current_time))
        (finish nil))
    (prog1 
      (funcall code)
      (setf finish (current_time))
      (time-stamp *standard-output*)
      (format t "~%Starting time: ~20T~A (~A)" (->hms-new start) (object-hash start))
      (format t "~%Finish time: ~20T~A (~A)" (->hms-new finish) (object-hash finish))
      (format t "~%Elapsed time: ~20T~A~%" (->hms-new (time-subtract finish start))))))

    
(defun ->hms-new (time)
  (let ((hour (ctime-hour time))
        (minute (ctime-minute time))
        (second (ctime-second time)))
    (concatenate 'string (number->string 
                          (cond ;;;;; ((zerop hour) 12)
                           ((<= hour 12) hour)
                           (t (- hour 12)))
                          2)
                 ":" (number->string minute 2)
                 ":" (number->string second 2))))
  

;; not your normal efficient tail-recursive procedure.
;; ;; runs twice as slow as regular fact due to bignum arithmetic overhead!
;; (define (tr-fact n)
;;   (labels (((tr-fact2 n result)
;;             (cond ((= n 0) result)
;;                   (else
;;                    (tr-fact2 (- n 1) (* n result))))))
;;     (tr-fact2 n 1)))
;; 
