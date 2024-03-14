

(defclass preference ()
  ((source   :initform nil :documentation "particular instance of member, bill, issue, group" 
             :accessor preference-source) 
   (source-db :initform nil :documentation "db of source"   :accessor preference-source-db) 
   (source-structure :initform nil :documentation "?"   :accessor preference-source-structure) 
   (relation :initform nil :documentation "?"   :accessor preference-relation) 
   (issue :initform nil :documentation "preference issue"   :accessor preference-issue) 
   (issue-structure :initform nil :documentation "?"   :accessor preference-issue-structure) 
   (importance :initform nil :documentation "preference importance (A, B, C, D)"   :accessor preference-importance) 
   (polarity :initform nil :documentation "preference polarity (high or low)"   :accessor preference-polarity) 
   (sort-key :initform nil :documentation "?"   :accessor preference-sort-key) 
   (siblings :initform nil :documentation "related preferences (must be preference-alikev?" :accessor preference-siblings) 
   ))

(defpredicate preference)

(defmethod all-the-slots ((self preference))
  '(source source-db source-structure relation issue issue-structure importance 
    polarity sort-key siblings))

(defmethod copy ((self preference))
  (copy-instance self))

(defmethod pretty-print ((self preference) stream)
  (print-hc self stream))

(defmethod print-object ((self preference) stream)
  (format stream "#<Preference (~A) [~A:~A] ~A (~A:~A)>"
          (object-hash self)
          (preference-importance self)
          (preference-polarity self)
          (preference-issue self)
          (id (preference-source-db self))
          (preference-source self)))

(defmethod english ((self preference))
  (eng-stance (stance-subject self) self))

(defmethod print-hc ((self preference) stream &rest args)
  (declare (ignore args))
  (let ((issue (preference-issue self))
        (polarity  (preference-polarity self))
        (db    (preference-source-db self))
        (source  (preference-source self))
        (importance (preference-importance self)))
    (format stream "(~A ~A ~A:~A ~A)"
            polarity importance db source issue)))

(defmethod print-readable2 ((self preference) stream)
  (format stream "(init-preference '(~A ~A ~A ~A ~A))~%"
          (preference-issue self)
          (preference-polarity self)
          (preference-importance self)
          (preference-source self)
          (id (preference-source-db self))))

(defmethod print-readable ((self preference) stream)
  (print-readable2 self stream))

(defmethod reveal-source ((self preference))
  (or (preference-source-structure self)
      (let ((i (type-check+fix (preference-source self)
                               (preference-source-db self)
                               'reveal-source)))
        (setf (preference-source-structure self) i))))

(defmethod reveal-issue ((self preference))
  (or (preference-issue-structure self)
      (let ((i (type-check+fix (preference-issue self) issue 'reveal-issue)))
        (setf (preference-issue-structure self) i))))

(defmethod inorder? ((self preference) (second preference))
  (inorder? (sort-key self)
            (sort-key second)))

(defmethod sort-key ((self preference))
  (if (preference-sort-key self)
    (preference-sort-key self)
    (->string (preference-importance self))))

(defmethod set-sort-key ((self preference) keyword)
  (let ((preference-import (preference-importance self))
        (rel-import (if (preference-relation self)
                      (relation-importance (preference-relation self))
                      'B)))
    (setf (preference-sort-key self)
          (apply #'string-append
                 (mapcar #'->string
                         (case keyword
                           ((loyalty) (list rel-import preference-import))
                           ((equity) (list preference-import rel-import))
                           ((imppolarity) (list preference-import (preference-polarity self)))
                           ((alpha) (list (reveal-issue self) preference-import rel-import))
                           (otherwise (error "Unknown keyword in SET-SORT-KEY: ~A" keyword))))))))

(defmethod match? ((self preference) (second preference))
  (let ((match-issue (match? (reveal-issue self)
                             (reveal-issue second)))
        (match-polarity (eq (preference-polarity self)
                            (preference-polarity second)))
        (match-import (eq (preference-importance self)
                          (preference-importance second))))
    (cond ((and match-issue
                match-polarity
                match-import)
           ;;`(ALL ,self ,second))
           second)
          ((and match-issue
                match-polarity)
           ;;`(PARTIAL ,self ,second))
           second)
          (t nil))))


(defun init-preference (arg)  
  (destructuring-bind (issue polarity importance source db) arg
    (let ((preference (make-instance 'preference))
          (s (check-arg #'isa-polarity? polarity 'init-preference))
          (i (check-arg #'isa-importance? importance 'init-preference)))
      (setf (preference-issue preference) issue)
      (setf (preference-source preference) source)
      (setf (preference-source-db preference) db)
      (setf (preference-polarity preference) s)
      (setf (preference-importance preference) i)
      preference)))

(defun isa-polarity? (p)
  (member p '(high low)))

(defun input-preference (source db &rest arg)  
  (let ((quant (cond (arg (car arg))
                     (t
                      (prompted-symbol-input "Quantity: ")))))
    (cond (quant
           ;; check to see if quantity is in database
           (let* ((quant-node (get-node quant quantity))
                  (quant-norm (if quant-node (quantity-norm quant-node) nil)))
             (format t "Input preference for quantity: ~A~%" quant)
             (if quant-norm
               (format t "Normative preference for ~A: ~%~10T~A~%"
                       quant quant-norm))
             (init-preference
              (list
               quant
               (verify-prompted-symbol-input "Polarity   (high/low): " '(high low))
               (verify-prompted-symbol-input "Importance (A-D): " '(A B C D))
               source
               db))))
          (t nil))))



(defun prompted-preference-list-input (prompt id db)  
  (print_prompt prompt)
  (format t " (Type RETURN to quit)~%")
  (let ((st (input-preference id db)))
    (if st
      (cons st (prompted-preference-list-input prompt id db))
      nil)))
