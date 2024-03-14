
;; support code for the shifting alliances decision strategy


(defun divided-groups (decision)  
  (let* ((no-credo (null (decision-mi-credo decision)))
         (beliefs (member-credo (decision-member decision)))
         (fors (msort (remove-intersection (decision-group-for decision)
                                           (decision-group-agn decision)
                                           #'stance-relation-alikev?)))
         (agns (msort (remove-intersection (decision-group-agn decision)
                                           (decision-group-for decision)
                                           #'stance-relation-alikev?)))
         )
    (cond ((and fors agns no-credo beliefs)
           (resolve-credo-conflicts decision))
          (else nil))))

(defun equal-stance-rel-import? (st1 st2)  
  (eq (stance-rel-import st1)
      (stance-rel-import st2)))

(defun stance-rel-import (stance)  
  (and (stance-p stance)
       (relation-p (stance-relation stance))
       (relation-importance (stance-relation stance))))

(defun stance-relation-alikev? (st1 st2)  
  (and (stance-p st1)
       (stance-p st2)
       (relation-alikev? (stance-relation st1)
                         (stance-relation st2))))


(defun resolve-credo-conflicts (decision)  
  (let* ((beliefs (member-credo (decision-member decision)))
         (fors (msort (remove-intersection (decision-group-for decision)
                                           (decision-group-agn decision)
                                           #'stance-relation-alikev?)))
         (agns (msort (remove-intersection (decision-group-agn decision)
                                           (decision-group-for decision)
                                           #'stance-relation-alikev?)))
         (for-conflicts (find-credo-conflicts beliefs fors))
         (agn-conflicts (find-credo-conflicts beliefs agns))
         )
    (cond ((and (null for-conflicts)
                (null agn-conflicts))
           nil)
          ((and for-conflicts agn-conflicts)
           (my-format t "~%Conflicts with BOTH sides.  No decision.~%")
           nil)
          (for-conflicts
           (my-format t "~%Conflict with FOR groups: ~%~5T~A~%"
                      for-conflicts)
           'agn)
          (agn-conflicts
           (my-format t "~%Conflict with AGN groups: ~%~5T~A~%"
                      agn-conflicts)
           'for))))


(defun find-credo-conflicts (beliefs stance-list)  
  (filter
   (mapcar 
    #'(lambda (st)
      (find-credo-stance-conflicts st beliefs))
    stance-list)
   #'null))

(defun find-credo-stance-conflicts (st beliefs)  
  (let ((grp (and (stance-p st)
                  (relation-group (stance-relation st)))))
    (if grp 
        (find-stance-conflicts beliefs (group-stances (get-node grp group)))
        nil)))

(defun find-stance-conflicts (st1 st2)  
  (find-intersection st1 st2 #'stance-opposite?))
  

(defun find-intersection (l1 l2 pred)  
  (collect
   l1
   #'(lambda (item)
     (member item l2 :test pred))))

(defun stance-opposite? (s1 s2)  
  (and (stance-p s1)
       (stance-p s2)
       (eq (reveal-issue s1)
           (reveal-issue s2))
       (not (eq (stance-side s1)
                (stance-side s2)))))

