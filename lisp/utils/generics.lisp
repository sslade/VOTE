;; generic method definitions

;; (id object)
;;------------------------------------------------
(defgeneric id (self)
  (:method (self) self)
  (:documentation "Returns the symbolic name of an object instance."))


;; (pretty-print object *standard-output*)
;;------------------------------------------------
(defgeneric pretty-print (self stream)
  (:method ((self hash-table) stream)
           (maphash
            #'(lambda (key value)
                (format stream "~A:~15T~A~%" key value))
            self))
  (:method (self stream) (format stream "~A" self))
  (:documentation "Method for pretty-printing objects."))


;; (print-readable object *standard-output*)
;;------------------------------------------------
(defgeneric print-readable (self stream)
  (:method ((self symbol) stream) (format stream " '~A" self))
  (:method ((self number) stream) (format stream " ~A" self))
  (:method ((self string) stream) (format stream " ~S" self))
  (:method ((self function) stream) (format stream " #'~S" (function-name self)))
  (:method ((self character) stream) (format stream " #\\~A" self))
  (:method ((self list) stream) 
           (format stream " (list ")
           (mapc #'(lambda (item) (print-readable item stream)) self)
           (format stream ")~%"))
  
  (:method (self stream)
           (declare (ignore stream))
           (error "bad type in PRINT-READABLE: ~A" self))
  (:documentation "Method for printing objects that can be read back."))


;; (print-readable2 object *standard-output*)
;;------------------------------------------------
(defgeneric print-readable2 (self stream)
  (:method ((self list) stream) (print-readable self stream))
  (:method (self stream) (format stream " '~S" (id self)))
  (:documentation "Method for printing objects that can be read back in stage 2."))


;; (print-header object *standard-output*)
;;------------------------------------------------
(defgeneric print-header (self stream &rest args)
  (:method (self stream &rest args) 
           (declare (ignore args)) 
           (print-object self stream))
  (:documentation "Method for printing output headers."))


