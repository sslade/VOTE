
(defclass relation ()
  ((source   :initform nil :documentation "particular instance of member, bill, issue, group, district" 
             :accessor relation-source) 
   (source-db :initform nil :documentation "db of source"   :accessor relation-source-db) 
   (source-structure :initform nil :documentation "?"   :accessor relation-source-structure) 
   (group :initform nil :documentation "relation group"   :accessor relation-group) 
   (group-structure :initform nil :documentation "?"   :accessor relation-group-structure) 
   (importance :initform nil :documentation "relation importance (A, B, C, D)"   :accessor relation-importance) 
   (side :initform nil :documentation "relation side (pro or con)"   :accessor relation-side) 
   ))

; (defpredicate relation)


(defmethod all-the-slots ((self relation))
  '(source source-db source-structure group group-structure importance side))

(defmethod copy ((self relation))
  (copy-instance self))

(defmethod pretty-print ((self relation) stream)
    (print-hc self stream))

(defmethod print-hc ((self relation) stream &rest args)
  (declare (ignore args))
  (let ((group (relation-group self))
        (side  (relation-side self))
        (source  (relation-source self))
        (db    (relation-source-db self))
        (importance (relation-importance self)))
    (format stream "(~A ~A ~A:~A ~A)"
            side importance db source group)))

(defmethod reveal-source ((self relation))
  (or (relation-source-structure self)
      (let ((i (type-check+fix (relation-source self)
                               (relation-source-db self)
                               'reveal-source)))
        (setf (relation-source-structure self) i))))

(defmethod print-object ((self relation) stream)
  (format stream "#{Relation (~A) [~A:~A] ~A (~A:~A)}"
          (object-hash self)
          (relation-importance self)
          (relation-side self)
          (relation-group self)
          (relation-source-db self)
          (relation-source self)))

(defmethod print-readable ((self relation) stream)
  (print-readable2 self stream))

(defmethod print-readable2 ((self relation) stream)
  (format stream "(init-relation '(~A ~A ~A ~A ~A))"
          (relation-group self)
          (relation-side self)
          (relation-importance self)
          (relation-source self)
          (relation-source-db self)))

(defmethod english ((self relation))
  (eng-relation (get-subject self) self))

(defmethod french ((self relation))
  (eng-relation (get-subject self) self))

(defmethod japanese ((self relation))
  (eng-relation (get-subject self) self))

(defmethod reveal-group ((self relation))
  (or (relation-group-structure self)
      (let ((i (check-arg #'group-p (get-node (relation-group self) group)
                          'reveal-group)))
        (setf (relation-group-structure self) i))))

(defmethod inorder? ((self relation) (second relation))
  (if (eq (relation-importance self)
          (relation-importance second))
    (inorder? (relation-side second)
              (relation-side self))
    (inorder? (relation-importance self)
              (relation-importance second))))


(defun init-relation (arg)  
  (let ((group (car arg))
        (side (cadr arg))
        (importance (caddr arg))
        (source (cadddr arg))
        (db (car (cddddr arg))))
    (let ((relation (make-instance 'relation))
          (s (check-arg #'isa-side? side 'init-relation))
          (i (check-arg #'isa-importance? importance 'init-relation)))
      (setf (relation-group relation) group)
      (setf (relation-source relation) source)
      (setf (relation-source-db relation) db)
      (setf (relation-side relation) s)
      (setf (relation-importance relation) i)
      relation)))


(defun input-relation (source db &rest arg)  
  (let ((grp (cond (arg (car arg))
                   (else
                    (prompted-symbol-input "Group: ")))))
    (cond (grp
           ;; check to see if group is in database
           (get-node grp group)
           (format t "~%Input relation for group: ~A~%" grp)
           (init-relation
            (list
             grp
             (verify-prompted-symbol-input "Side   (pro/con): " '(pro con))
             (verify-prompted-symbol-input "Importance (A-D): " '(A B C D))
             source
             db)))
          (else nil))))

(defun prompted-relation-list-input (prompt id db)  
  (print_prompt prompt)
  (format t " (Type RETURN to quit)~%")
  (let ((rel (input-relation id db)))
    (if rel
      (cons rel (prompted-relation-list-input prompt id db))
      nil)))

;;;    
;;;   (set rtest
;;;        (list
;;;         (init-relation '(blacks pro b bruce))
;;;         (init-relation '(bankers con b bruce))
;;;         (init-relation '(labor pro a bruce))
;;;         (init-relation '(jew pro b bruce))
;;;         ))
;;;   
