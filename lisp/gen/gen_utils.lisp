
;;  A stance is a norm if it is equal to the stance filling the norm
;;  slot of its issue.
;;------------------------------------------------------------------
(defun norm? (stance)  
  (cond ((stance-p stance)
         (eq (issue-norm (reveal-issue stance)) stance))
        ((relation-p stance)
         (eq (group-norm (reveal-group stance)) stance))
        (else nil)))


;; pick a random phrase from a list of phrases.
;;
;; E.g.,
;; (gen-pick-one "one" "two" "three")

(defun gen-pick-one (&rest choices)  
  (let* ((range (length choices)))
    (nth (random range) choices)))



;; infer a subject for a stance or relation

(defun get-subject (stance)  
  (let ((source (reveal-source stance)))
    (cond ((norm? stance) (gen-norm-np))
          ((member-p source) source)
          ((group-p source) source)
          ((bill-p source) *record*)
          (else nil))))

        
;; 9/9/16
;; (function-synonym stance-subject get-subject)
