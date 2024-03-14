
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
(defvar agent)

(defclass agent (record)
  ((name     :initform nil :documentation "name of agent" :accessor agent-name) 
   (english  :initform nil :documentation "English version of agent name" :accessor agent-english)
   (sort-key :initform nil :documentation "agent Sort Key" :accessor agent-sort-key)
   (synonyms :initform nil :documentation "agent synonyms - list of symbols" :accessor agent-synonyms)
   (isa      :initform nil :documentation "List of symbols" :accessor agent-isa)
   (isa-depth :initform nil :documentation "String for padding printing" :accessor agent-isa-depth)
   (instances :initform nil :documentation "List of agents" :accessor agent-instances)
   (importance :initform nil :documentation "Intrinsic importance of agent" :accessor agent-importance)
   (preferences :initform nil :documentation "Preferences for agent" 
                :accessor agent-preferences)
   (notes     :initform nil :documentation "List of remarks" :accessor agent-notes))
  (:documentation "agent Class"))

(defpredicate agent)

(defmethod all-the-slots ((obj agent))
  (append '(name english sort-key synonyms isa isa-depth instances importance
            preferences notes)
          (call-next-method)))


(defmethod print-object ((self agent) stream)
  (format stream "#<Agent (~A) ~A>"
          (object-hash self) 
          (agent-name self)))
   
(defmethod english ((self agent))
  (cond ((agent-english self))
        (t
         (concatenate 'string "**the " (agent-name self) "**"))))

(defmethod english-short ((self agent))
  (agent-name self))

(defmethod inorder? ((self agent) (other agent))
  (if (agent-p other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (cond ((and (date? first) (date? second))
             (inorder? first second))
            (t
             (string< first second))))
    nil))

(defmethod synonyms ((self agent))
  (let ((name (read-string->list (agent-name self))))
    (if (= (length name) 1)
      (append name (agent-synonyms self))
      (agent-synonyms self))))

(defmethod sort-key ((self agent))
  (if (agent-sort-key self)
    (agent-sort-key self)
    (agent-name self)))

(defmethod set-isa-sort ((self agent))
  (setf (agent-isa-depth self) (isa-depth self agent))
  (setf (agent-sort-key self) (generate-isa-sort-key self agent)))

(defmethod set-alpha-sort ((self agent))
  (setf (agent-isa-depth self) "")
  (setf (agent-sort-key self) (agent-name self)))

(defmethod id ((self agent))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'agent)))
        (t (record-symbol self))))
   
(defmethod print-readable2 ((self agent) stream)
  (format stream "~%(get-node ~A agent)"
          (id self)))

(defmethod print-header ((self agent) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~18T[~A]~28T ~A~50T~A"
          (case (char (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (agent-importance self)
          (agent-name self)
          (cond ((agent-preferences self))
                (t ""))))


(defmethod print-hc ((self agent) stream &rest args)
    (declare (ignore args))
    (format stream "\\hinge{}~%[~A] ~15T{\\rm ~A}~%"
            (agent-importance self)
            (agent-name self))
    (mapc
     #'(lambda (sel-id)
         (let ((selector (car sel-id))
               (identifier (cdr sel-id)))
           (if (funcall selector self)
             (progn 
               (format stream "~%\\H ~A~26T"
                       identifier)
               (pretty-print (funcall selector self) stream)
               ))))
     (list
      (cons #'agent-preferences "Preferences:")
      ))
    
    )
             

(defmethod pretty-print ((self agent) stream)
  (call-next-method)
  (cond ((agent-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (agent-notes self))))
  )


(defmethod input-prototype ((self agent) slot)
  (case slot
    ((isa-depth)     #'(LAMBDA () ""))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((preferences)   #'(LAMBDA () (prompted-preference-list-input "Preferences: " (id self) agent)))
    (otherwise       (call-next-method))))

 

(defmethod update-prototype ((self agent) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((preferences)   #'(LAMBDA () (prompted-preference-list-input "Preferences: " (id self) agent)))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (agent-notes self))))
    (otherwise       (call-next-method))))


(defmethod no-print-readable-fields ((self agent))
  (append '(sort-key instances isa-depth) (call-next-method)))

(defmethod table-index-fields ((self agent))
  (append '(synonyms name keywords isa) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of agents
;;----------------------------------------------------------

;; agent is not defvar'd in db.lisp
(defvar agent)
(setq agent (make-db)) 
(setf (db-name agent) 'agent) 
(setf (db-prompt agent) "agent> ")
(setf (db-class agent) 'agent)
(setf (db-commands agent)
      (append
       '((notes (notes note))
         (name (name))
         (isa (isa))
         (english (english))
         (synonyms (synonyms synonym names))
         (preferences (preferences prefer))
         )
       *record-db-commands*))

(init-db agent)



(defun get-preference (agt quant)
  (if (not (agent-p agt))
    (setf agt (get-node agt agent)))
  (if (not (quantity-p quant))
    (setf quant (get-node quant quantity)))
  (let ((prefs (agent-preferences agt)))
    (cond ((find-if #'(lambda (p) (eq quant (get-node (preference-issue p) quantity))) prefs))
          (t
           (infer-preference-from-formula agt quant)))))


(defun get-preference-polarity (agt quantity)
  (preference-polarity (get-preference agt quantity)))

(defun get-preference-importance (agt quantity)
  (preference-importance (get-preference agt quantity)))


(defun infer-preference-from-formula (agt quant)
  (let ((form (quantity-formula quant)))
    (cond ((null form)
           (infer-norm quant))
          ((atom form)
           (get-preference agt form))
          (t
           (destructuring-bind (left operator right) form 
             (qbc-arithmetic (get-preference agt left)
                             operator
                             (get-preference agt right)))))))

