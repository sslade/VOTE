;;; object-hash.cl

;; better to encapsulate this table to prevent unintended side-effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *object-hash-table* (make-hash-table))
 
(setf (get '*object-hash-table* 'counter) 0)

(defun genhash ()
  (prog1 (get '*object-hash-table* 'counter)
    (incf (get '*object-hash-table* 'counter))))


(defun old-object-hash (obj)
  (cond ((gethash (sxhash obj) *object-hash-table*))
        (else
         (let ((gtemp (genhash)))
           (setf 
            (gethash (sxhash obj) *object-hash-table*)
            gtemp)
           (setf (gethash gtemp *object-hash-table*) obj)
           gtemp))))


(defun old-object-unhash (gtemp)
  (gethash gtemp *object-hash-table*))

(defun object-hash (obj)
  (let ((sym
         (if (symbolp obj)
           obj
           (convert-to-symbol obj))))
    (cond ((get sym '*hash-counter*))
          (else
           (let ((gtemp (genhash)))
             (setf (get sym '*hash-counter*) gtemp)
             (setf (gethash gtemp *object-hash-table*) obj)
             gtemp)))))


(defun object-unhash (gtemp)
  (gethash gtemp *object-hash-table*))


(defun convert-to-symbol (obj)
  (setf obj (->symbol obj))
  (cond ((symbolp obj) obj)
        ((gethash obj *object-hash-table*))
        (else
         (setf (gethash obj *object-hash-table*) (gensym "HASH")))))


        
        