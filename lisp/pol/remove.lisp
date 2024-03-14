
(defun remove-intersection (l1 l2 pred)  
  (filter
   l1
   #'(lambda (item)
     (member item l2 :test pred))))


(setf (symbol-function 'r-i) (symbol-function 'remove-intersection))

(defun stance-equal? (s1 s2)  
  (and (stance-alikev? s1 s2)
       (eq (stance-importance s1)
           (stance-importance s2))))

(defun stance-alikev? (s1 s2)  
  (and (stance-p s1)
       (stance-p s2)
       (eq (reveal-issue s1)
           (reveal-issue s2))
       (eq (stance-side s1)
           (stance-side s2))))


(defun relation-alikev? (r1 r2)  
  (and (relation-p r1)
       (relation-p r2)
       (eq (reveal-group r1)
           (reveal-group r2))
       (eq (relation-side r1)
           (relation-side r2))))


            
