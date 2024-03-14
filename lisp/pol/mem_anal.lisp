
;;  This file contains procedures for analyzing members' stances and
;;  voting records.  There are two main routines: mdiff and consistent?
;;  


;---------------------------------------------------
;   (mdiff mem1 mem2)
;---------------------------------------------------
;; compare the voting records of two members.
;;

(defun mdiff (mem1 mem2)  
  (let* ((m1 (get-node mem1 member))
         (m2 (get-node mem2 member))
         (v1 (msort (member-votes m1)))
         (v2 (msort (member-votes m2))))
    (print-diffs v1 v2)))

(defun print-diffs (l1 l2)  
  (cond ((and (null l1)
              (null l2))
         nil)
        ((null l1)
         (mapc #'(lambda (item) (print-missing '2nd item))
               l2))
        ((null l2)
         (mapc #'(lambda (item) (print-missing '1st item))
               l1))
        ((diff-match l1 l2))
        (else nil)))
         
    

(defun print-missing (lname item)  
  (format t "~%~A missing item: ~A"
          lname item))

(defun print-different (item1 item2)  
  (format t "~% 1st: ~A ~% 2nd: ~A"
           item1 item2))


(defun diff-match (l1 l2)  
  (let ((item1 (car l1))
        (item2 (car l2)))
    (cond ((diff-match? item1 item2)
           (print-diffs (cdr l1) (cdr l2)))
          ((partial-match? item1 item2)
           (print-different item1 item2)
           (print-diffs (cdr l1) (cdr l2)))
          ((collect (cdr l2)
                    #'(lambda (item) (partial-match? item1 item)))
           (print-missing '1st item2)
           (print-diffs l1 (cdr l2)))
          ((collect (cdr l1)
                    #'(lambda (item) (partial-match? item2 item)))
           (print-missing '2nd item1)
           (print-diffs (cdr l1) l2))
          (else
           (print-missing '1st item2)
           (print-missing '2nd item1)
           (print-diffs (cdr l1) (cdr l2))))))


(defun diff-match? (x y) (equalp x y))

(defun partial-match? (l1 l2)  
  (equalp (car l1) (car l2)))


;---------------------------------------------------
;   (consistent? mem)
;---------------------------------------------------
;; detect inconsistencies in a member's relations, credo, 
;; and voting record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consistent? (mem)  
  (let* ((m (get-node mem member))
         (stances
          (progn 
            (setf (member-stances m) (extract-voting-stances m))
            (infer-member-rel-stances m)            
            (append (member-credo m)
                    (member-stances m)
                    (member-pro-rel-stances m)))))
    (format t "~%Member: ~A~%" (member-name m))
    (mapc 
     #'(lambda (st) (set-sort-key st 'alpha))
     stances)
    (filter-stances (msort stances) 0)))


(defun filter-stances (stances count)  
  (let ((first (car stances))
        (second (cadr stances)))
    (cond ((null second)
           count)
          ((eq (reveal-issue first)
               (reveal-issue second))
           (cond ((eq (stance-side first)
                      (stance-side second))
                  (filter-stances (cdr stances) count))
                 (else
                  (format t "~% ~A~% ~A~%"
                          first
                          second)
                  (filter-stances (cdr stances) (1+ count)))))
          (else
           (filter-stances (cdr stances) count)))))

