
;; cluster stances and relations for English output
;; e.g.,
;;         A, B, and C

(defun cluster-objs (obj-list)  
  (if (stance-p (car obj-list))
      (mapc 
       #'(lambda (stance)
         (set-sort-key stance 'impside))
       obj-list))
  (let ((sl (msort obj-list)))
    (cluster-objs2 sl nil)))

(defun cluster-objs2 (obj-list cluster)  
  (cond ((null obj-list) (list cluster))
        ((null cluster)
         (cluster-objs2 (cdr obj-list) (list (car obj-list))))
        ((imp-side-eq? (car obj-list) (car cluster))
         (cluster-objs2 (cdr obj-list) (cons (car obj-list)
                                                   cluster)))
        (else
         (cons cluster 
               (cluster-objs2 (cdr obj-list) (list (car obj-list)))))))


;; works for stances or relations

(defun imp-side-eq? (obj1 obj2)  
  (and (eq (get-importance obj1)
            (get-importance obj2))
       (eq (get-side obj1)
            (get-side obj2))))



(defun say-cluster-reasons (subj reasons stream)  
  (if reasons
      (paragraph
       (mapcar #'(lambda (cluster) (say-reason-clusters subj cluster))
            (cluster-objs reasons))
       stream)))


(defun say-reason-clusters (subject stance-list)  
  (if (null (cdr stance-list))
      (eng-stance subject (car stance-list))
      (concatenate 'string (gen-np subject 'subj)
                     (eng-stance2 subject (car stance-list))
                     (eng-stance-clusters stance-list)
                     ".")))
  
(defun eng-stance-clusters (stance-list)  
  (cond ((null (cdr stance-list))
         (concatenate 'string (gen-conjunction)
                        (eng-stance-issue (car stance-list))))
        (else
         (concatenate 'string (eng-stance-issue (car stance-list))
                        ", "
                        (eng-stance-clusters (cdr stance-list))))))


(defun gen-conjunction ()
  (gen-pick-one
   "and "
   "and "
   "as well as "
   "in addition to "
   "and moreover "
   ))
