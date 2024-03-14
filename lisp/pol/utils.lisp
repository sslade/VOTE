

(defun isa-side? (side)  
  (MEMBER side '(pro con)))

(defun other-side (side)  
  (case side
    ((pro) 'con)
    ((con) 'pro)))

(defun isa-importance? (importance)  
  (member importance '(a b c d)))

(defun >=important? (imp1 imp2)
  (or (eq imp1 imp2)
      (inorder? imp1 imp2)))

(defun >important? (imp1 imp2)  
  (and (not (eq imp1 imp2))
       (inorder? imp1 imp2)))

(defun <important? (imp1 imp2)  
  (not (>=important? imp1 imp2)))

(defun most-important? (importance)  
  (eq importance 'a))

(defun opposite-result (result)  
  (case result
    ((agn) 'for)
    ((for) 'agn)
    (else nil)))

;;------------------------------------------------------------------
;;  type-check+fix  
;;------------------------------------------------------------------

;; if dbase parameter is nil, then arg must be evaluated 

(defun type-check+fix (arg dbase proc)  
  (cond ((listp dbase)
         (return-first dbase #'(lambda (db) (symbol->structure arg db))))
        (dbase
         (cond ((symbol->structure arg dbase))
               (else
                (error "Type mismatch for ~A in ~A.  Should be ~A.~%"
                       arg proc dbase))))
        ((instance-p arg)
         arg)
        (else
         (eval arg))))

(defun symbol->structure (arg dbase)  
  (if (not (db-p dbase))
      (setf dbase (*db-table* dbase)))
  (let ((class (db-class dbase)))
    (cond ((eq class (type-of arg)) arg)
          ((get-node arg dbase))
          (else nil))))


;;; ? same as some ?
(defun return-first (args proc)  
  (cond ((null args) nil)
        ((funcall proc (car args)))
        (else
         (return-first (cdr args) proc))))

;;;;------------------------------------------------------------------------
;;;;    utility procedures
;;;;------------------------------------------------------------------------

(defun lastchar (string)  
    (cond ((string= "" string) string)
          (else
           (elt string (- (length string) 1)))))


;  slightly more efficient (when compiled)
(defun string-reverse (text)  
    (coerce (string-reverse2 text '()) 'string))  

(defun string-reverse2 (text result)  
    (cond ((string= "" text) result)
          (else 
           (string-reverse2 (subseq text 1) 
                            (cons (char text 0) 
                                  result)))))

;(defun remove-duplicates (l)  
;  (cond ((null l) nil)
;        (else
;         (cons (car l)
;               (remove-duplicates
;                (filter l #'(lambda (n) (eq n (car l)))))))))


(defun keep-duplicates (lst)  
  (cond ((null lst) nil)
        ((member (car lst) (cdr lst))
         (cons (car lst) (keep-duplicates (cdr lst))))
        (else
         (keep-duplicates (cdr lst)))))
