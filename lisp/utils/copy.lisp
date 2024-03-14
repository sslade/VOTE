
(defun class-slot-names (class-name)
  (mapcar #'slot-definition-name
	  (class-class-slots (find-class class-name))))

(defgeneric copy (object)
  (:method ((object list)) (copy-tree object))
;;;  (:method ((object list)) (copy-list object))

  (:method ((object sequence)) (copy-seq object))
  (:method ((object symbol)) (copy-symbol object)))

;; mcl already has a copy-instance

(defun my-copy-instance (object)
  (let* ((class (class-of object))
         (slots (class-slot-names (class-name class)))
         (newobj (make-instance class)))
    (mapc #'(lambda (slot)
              (setf (slot-value newobj slot) 
                    (slot-value object slot)))
          slots)
    newobj))




