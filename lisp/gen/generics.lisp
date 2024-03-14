
;; (instance-p object)
;; used in place of structure? in T
;; --------------------------------
(defgeneric instance-p (self)
  (:method (self) nil)
;;  (:method ((self record)) T)
  (:method ((self np)) T)
  (:documentation "predicate for class instances, e.g., dbase records and other stuff"))

