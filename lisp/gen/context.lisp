
;; create a context for the current generation episode


(defclass context ()
  ((np-table :initform nil :accessor context-np-table 
             :documentation "table of noun phrases that can be pronominalized or not")
   (np-stack :initform nil :accessor context-np-stack
             :documentation "stack of noun phrases")
   (pronoun-cycle :initform nil :accessor context-pronoun-cycle
                  :documentation "number of times to use pronoun before restating noun")
   (subject  :initform nil :accessor context-subject
             :documentation "np subject of current sentence")
   (speaker  :initform nil :accessor context-speaker)
   (hearer   :initform nil :accessor context-hearer))
  (:documentation "class for generation context"))

(defmethod all-the-slots ((obj context))
  '(np-table np-stack pronoun-cycle subject speaker hearer))

(defmethod print-object ((self context) stream)
  (format stream "#{Context (~A)}"
          (object-hash self)))

(defmethod pretty-print ((self context) stream)
  (mapc
   #'(lambda (slot)
       (let ((value (slot-value self slot)))
         (cond ((null value)
                nil)

               ((eq slot 'np-table)
                (format stream "~%~A:" (capitalize slot))
                (maphash
                 #'(lambda (k v)
                     (format stream "~2T~A~10T~A~%" v (or (english k) k)))
                 value))
             
               (else
                (format stream "~%~A: ~15T"
                        (capitalize slot))
                (print value stream)))))
   (all-the-slots self)))

(defmethod said? ((self context) np)
  (prog1 
     (member np (context-np-stack self))
     (push np (context-np-stack self))))

(defmethod use-noun ((self context) np)
  (setf (gethash np (context-np-table self)) nil)
  nil)
   
(defmethod set-pronoun-count ((self context) np count)
  (setf (gethash np (context-np-table self)) count))

(defmethod use-pronoun? ((self context) np)
  (prog1 
    (cond ((eq 'always (gen-get-feature np 'use-pronoun))
           t)
          ((eq 'always (gen-get-feature np 'use-noun))          ; 
           nil)
          ((gethash np (context-np-table self))
           (let ((count (gethash np (context-np-table self))))
             (cond ((zerop count)
                    (use-noun self np))
                   (else
                    (set-pronoun-count self np (1+ count))))))
          (else
           (set-pronoun-count self np (- (context-pronoun-cycle self)))
           nil))
    ))



(setf *context* nil) 
(defun init-context ()
  (setf *context* (make-instance 'context))
  (setf (context-np-table *context*) (make-hash-table))
  (setf (context-pronoun-cycle *context*) 2)
  (setf (context-np-stack *context*) nil)
  )

(init-context)
