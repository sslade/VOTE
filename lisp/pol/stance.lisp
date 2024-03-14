

(defclass stance ()
  ((source   :initform nil :documentation "particular instance of member, bill, issue, group" 
             :accessor stance-source) 
   (source-db :initform nil :documentation "db of source"   :accessor stance-source-db) 
   (source-structure :initform nil :documentation "?"   :accessor stance-source-structure) 
   (relation :initform nil :documentation "?"   :accessor stance-relation) 
   (issue :initform nil :documentation "stance issue"   :accessor stance-issue) 
   (issue-structure :initform nil :documentation "?"   :accessor stance-issue-structure) 
   (importance :initform nil :documentation "stance importance (A, B, C, D)"   :accessor stance-importance) 
   (side :initform nil :documentation "stance side (pro or con)"   :accessor stance-side) 
   (sort-key :initform nil :documentation "?"   :accessor stance-sort-key) 
   (siblings :initform nil :documentation "related stances (must be stance-alikev?" :accessor stance-siblings) 
   ))

; (defpredicate stance)

(defmethod all-the-slots ((self stance))
  '(source source-db source-structure relation issue issue-structure importance 
    side sort-key siblings))

(defmethod copy ((self stance))
  (copy-instance self))

(defmethod pretty-print ((self stance) stream)
    (print-hc self stream))

(defmethod print-object ((self stance) stream)
  (format stream "#{Stance (~A) [~A:~A] ~A (~A:~A)}"
          (object-hash self)
          (stance-importance self)
          (stance-side self)
          (stance-issue self)
          (stance-source-db self)
          (stance-source self)))

(defmethod english ((self stance))
  ;; 9/9/16
  ;; (eng-stance (stance-subject self) self))
  (eng-stance (get-subject self) self))

(defmethod print-hc ((self stance) stream &rest args)
  (declare (ignore args))
  (let ((issue (stance-issue self))
        (side  (stance-side self))
        (db    (stance-source-db self))
        (source  (stance-source self))
        (importance (stance-importance self)))
    (format stream "(~A ~A ~A:~A ~A)"
            side importance db source issue)))

(defmethod print-readable2 ((self stance) stream)
  (format stream "(init-stance '(~A ~A ~A ~A ~A))~%"
          (stance-issue self)
          (stance-side self)
          (stance-importance self)
          (stance-source self)
          (stance-source-db self)))

(defmethod print-readable ((self stance) stream)
  (print-readable2 self stream))

(defmethod reveal-source ((self stance))
  (or (stance-source-structure self)
      (let ((i (type-check+fix (stance-source self)
                               (stance-source-db self)
                               'reveal-source)))
        (setf (stance-source-structure self) i))))

(defmethod reveal-issue ((self stance))
  (or (stance-issue-structure self)
      (let ((i (type-check+fix (stance-issue self) issue 'reveal-issue)))
        (setf (stance-issue-structure self) i))))

(defmethod inorder? ((self stance) (second stance))
  (inorder? (sort-key self)
            (sort-key second)))

(defmethod sort-key ((self stance))
  (if (stance-sort-key self)
    (stance-sort-key self)
    (->string (stance-importance self))))

(defmethod set-sort-key ((self stance) keyword)
  (let ((stance-import (stance-importance self))
        (rel-import (if (stance-relation self)
                      (relation-importance (stance-relation self))
                      'B)))
    (setf (stance-sort-key self)
          (apply #'string-append
                 (mapcar #'->string
                         (case keyword
                           ((loyalty) (list rel-import stance-import))
                           ((equity) (list stance-import rel-import))
                           ((impside) (list stance-import (stance-side self)))
                           ((alpha) (list (reveal-issue self) stance-import rel-import))
                           (otherwise (error "Unknown keyword in SET-SORT-KEY: ~A" keyword))))))))

(defmethod match? ((self stance) (second stance))
  (let ((match-issue (match? (reveal-issue self)
                             (reveal-issue second)))
        (match-side (eq (stance-side self)
                         (stance-side second)))
        (match-import (eq (stance-importance self)
                           (stance-importance second))))
    (cond ((and match-issue
                match-side
                match-import)
           ;;`(ALL ,self ,second))
           second)
          ((and match-issue
                match-side)
           ;;`(PARTIAL ,self ,second))
           second)
          (else nil))))

 
(defun init-stance (arg)  
  (let ((issue (car arg))
        (side (cadr arg))
        (importance (caddr arg))
        (source (cadddr arg))
        (db (car (cddddr arg))))
    (let ((stance (make-instance 'stance))
          (s (check-arg #'isa-side? side 'init-stance))
          (i (check-arg #'isa-importance? importance 'init-stance)))
      (setf (stance-issue stance) issue)
      (setf (stance-source stance) source)
      (setf (stance-source-db stance) db)
      (setf (stance-side stance) s)
      (setf (stance-importance stance) i)
      stance)))

(defun check-arg (pred val calling-fn)
  (if (funcall pred val)
    val
    (error "~%CHECK-ARG: Bad value: ~A in function: ~A" val calling-fn)))


(defun input-stance (source db &rest arg)  
  (let ((iss (cond (arg (car arg))
                   (else
                    (prompted-symbol-input "Issue: ")))))
    (cond (iss
           ;; check to see if issue is in database
           (let* ((iss-node (get-node iss issue))
                  (iss-norm (if iss-node (issue-norm iss-node) nil)))
             (format t "Input stance for issue: ~A~%" iss)
             (if iss-norm
                 (format t "Normative stance for ~A: ~%~10T~A~%"
                         iss iss-norm))
             (init-stance
              (list
               iss
               (verify-prompted-symbol-input "Side   (pro/con): " '(pro con))
               (verify-prompted-symbol-input "Importance (A-D): " '(A B C D))
               source
               db))))
          (else nil))))



(defun verify-prompted-symbol-input (string options)  
  (let ((input (prompted-symbol-input string)))
    (cond ((member input options)
           input)
          (else
           (format t "~%Bad input.  Must be one of: ~A~%"
                   options)
           (verify-prompted-symbol-input string options)))))


(defun prompted-stance-list-input (prompt id db)  
  (print_prompt prompt)
  (format t " (Type RETURN to quit)~%")
  (let ((st (input-stance id db)))
     (if st
       (cons st (prompted-stance-list-input prompt id db))
       nil)))

(defun bill-stance-update (bill-id)  
  (let ((for (bill-stance-for bill-id))
        (agn (bill-stance-agn bill-id))
        (id  (bill-bnumber bill-id)))
    (setf (stance-source for) id)
    (setf (stance-source agn) id)))


    
