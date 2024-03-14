

(defclass datum ()
  ((source   :initform nil :documentation "particular instance of member, bill, issue, group" 
             :accessor datum-source) 
   (source-db :initform nil :documentation "db of source"   :accessor datum-source-db) 
   (source-structure :initform nil :documentation "?"   :accessor datum-source-structure) 
   (relation :initform nil :documentation "?"   :accessor datum-relation) 
   (issue :initform nil :documentation "datum issue"   :accessor datum-issue) 
   (issue-structure :initform nil :documentation "?"   :accessor datum-issue-structure) 
   (value :initform nil :documentation "datum value (high or low)"   :accessor datum-value) 
   (sort-key :initform nil :documentation "?"   :accessor datum-sort-key) 
   ))

(defpredicate datum)

(defmethod all-the-slots ((self datum))
  '(source source-db source-structure relation issue issue-structure 
    value sort-key ))

(defmethod copy ((self datum))
  (copy-instance self))

(defmethod pretty-print ((self datum) stream)
  (print-hc self stream))

(defmethod print-object ((self datum) stream)
  (format stream "\\verb|#{Datum (~A) [~A] ~A (~A:~A)}|"
          (object-hash self)
          (datum-value self)
          (datum-issue self)
          (id (datum-source-db self))
          (datum-source self)))

(defmethod english ((self datum))
  (eng-stance (stance-subject self) self))

(defmethod print-hc ((self datum) stream &rest args)
  (declare (ignore args))
  (let ((issue (datum-issue self))
        (value  (datum-value self))
        (db    (datum-source-db self))
        (source  (datum-source self))
        )
    (format stream "(~A ~A ~A:~A ~A)"
            value  db source issue)))

(defmethod print-readable2 ((self datum) stream)
  (format stream "(init-datum '(~A ~A ~A ~A))~%"
          (datum-issue self)
          (datum-value self)
          (datum-source self)
          (id (datum-source-db self))))

(defmethod print-readable ((self datum) stream)
  (print-readable2 self stream))

(defmethod reveal-source ((self datum))
  (or (datum-source-structure self)
      (let ((i (type-check+fix (datum-source self)
                               (datum-source-db self)
                               'reveal-source)))
        (setf (datum-source-structure self) i))))

(defmethod reveal-issue ((self datum))
  (or (datum-issue-structure self)
      (let ((i (type-check+fix (datum-issue self) issue 'reveal-issue)))
        (setf (datum-issue-structure self) i))))

(defmethod inorder? ((self datum) (second datum))
  (inorder? (sort-key self)
            (sort-key second)))

(defmethod sort-key ((self datum))
  (if (datum-sort-key self)
    (datum-sort-key self)
    (->string (datum-issue self))))

(defmethod match? ((self datum) (second datum))
  (let ((match-issue (match? (reveal-issue self)
                             (reveal-issue second)))
        (match-value (eq (datum-value self)
                         (datum-value second))))
    (cond ((and match-issue
                match-value)
           ;;`(ALL ,self ,second))
           second)
          ((and match-issue
                match-value)
           ;;`(PARTIAL ,self ,second))
           second)
          (t nil))))

 
(defun init-datum (arg)  
  (let ((issue (car arg))
        (value (cadr arg))
        (source (caddr arg))
        (db (car (cdddr arg))))
    (let ((datum (make-instance 'datum))
          (s (check-arg #'numberp value 'init-datum)))
      (setf (datum-issue datum) issue)
      (setf (datum-source datum) source)
      (setf (datum-source-db datum) db)
      (setf (datum-value datum) s)
      datum)))

(defun input-datum (source db &rest arg)  
  (let ((quant (cond (arg (car arg))
                     (t
                      (prompted-symbol-input "Quantity: ")))))
    (cond (quant
           ;; check to see if quantity is in database
           (let* ((quant-node (get-node quant quantity))
                  (quant-norm (if quant-node (quantity-norm quant-node) nil)))
             (format t "Input datum value for quantity: ~A~%" quant)
             (if quant-norm
               (format t "Normative datum for ~A: ~%~10T~A~%"
                       quant quant-norm))
             (init-datum
              (list
               quant
               (prompted-symbol-input "Value: ")
               source
               db))))
          (t nil))))



(defun prompted-datum-list-input (prompt id db)  
  (print_prompt prompt)
  (format t " (Type RETURN to quit)~%")
  (let ((st (input-datum id db)))
    (if st
      (cons st (prompted-datum-list-input prompt id db))
      nil)))
