
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
(defvar quantity)

(defclass quantity (record)
  ((name     :initform nil :documentation "name of quantity" :accessor quantity-name) 
   (english  :initform nil :documentation "English version of quantity name" :accessor quantity-english)
   (sort-key :initform nil :documentation "quantity Sort Key" :accessor quantity-sort-key)
   (synonyms :initform nil :documentation "quantity synonyms - list of symbols" :accessor quantity-synonyms)
   (isa      :initform nil :documentation "List of symbols" :accessor quantity-isa)
   (isa-depth :initform nil :documentation "String for padding printing" :accessor quantity-isa-depth)
   (instances :initform nil :documentation "List of quantities" :accessor quantity-instances)
   (importance :initform nil :documentation "Intrinsic importance of quantity" :accessor quantity-importance)
   (norm      :initform nil :documentation "Intrinsic polarity of quantity: high or low" :accessor quantity-norm)
   (dimension :initform nil :documentation "Dimension of quantity" 
              :accessor quantity-dimension)
   (formula   :initform nil :documentation "Formula for calculating this quantity" 
              :accessor quantity-formula)
   (notes     :initform nil :documentation "List of remarks" :accessor quantity-notes))
  (:documentation "quantity Class"))

(defpredicate quantity)

(defmethod all-the-slots ((obj quantity))
  (append '(name english sort-key synonyms isa isa-depth instances importance
            norm dimension formula notes)
          (call-next-method)))


(defmethod print-object ((self quantity) stream)
  (format stream "\\verb|#{Quantity (~A) ~A}|"
          (object-hash self) 
          (quantity-name self)))

(defmethod english ((self quantity))
  (cond ((quantity-english self))
        (t
         (concatenate 'string "**the " (quantity-name self) "**"))))

(defmethod english-short ((self quantity))
  (quantity-name self))

(defmethod inorder? ((self quantity) (other quantity))
  (if (quantity-p other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (cond ((and (date? first) (date? second))
             (inorder? first second))
            (t
             (string< first second))))
    nil))

(defmethod synonyms ((self quantity))
  (let ((name (read-string->list (quantity-name self))))
    (if (= (length name) 1)
      (append name (quantity-synonyms self))
      (quantity-synonyms self))))

(defmethod sort-key ((self quantity))
  (if (quantity-sort-key self)
    (quantity-sort-key self)
    (quantity-name self)))

(defmethod set-isa-sort ((self quantity))
  (setf (quantity-isa-depth self) (isa-depth self quantity))
  (setf (quantity-sort-key self) (generate-isa-sort-key self quantity)))

(defmethod set-alpha-sort ((self quantity))
  (setf (quantity-isa-depth self) "")
  (setf (quantity-sort-key self) (quantity-name self)))

(defmethod id ((self quantity))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'quantity)))
        (t (record-symbol self))))

(defmethod print-readable2 ((self quantity) stream)
  (format stream "~%(get-node ~A quantity)"
          (id self)))

(defmethod print-header ((self quantity) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~18T[~A] ~A~28T ~A~50T~A"
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
          (quantity-importance self)
          (cond ((quantity-norm self))
                (t (format nil "~A*" (infer-norm self))))
          (quantity-name self)
          (cond ((quantity-formula self))
                (t ""))))


(defmethod print-hc ((self quantity) stream &rest args)
  (declare (ignore args))
  (format stream "\\hinge{}~%[~A] ~15T{\\rm ~A}~%"
          (quantity-importance self)
          (quantity-name self))
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
    (cons #'quantity-dimension "Dimension:")
    (cons #'quantity-formula "Formula:")
    ))
  
  )


(defmethod pretty-print ((self quantity) stream)
  (call-next-method)
  (cond ((quantity-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (quantity-notes self))))
  )


(defmethod input-prototype ((self quantity) slot)
  (case slot
    ((isa-depth)     #'(LAMBDA () ""))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((formula)       #'(LAMBDA () (prompted-list-input "Formula: ")))
    ((dimension)     #'(LAMBDA () (prompted-list-input "Dimension: ")))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    ((norm)          #'(LAMBDA () (prompted-symbol-input "Norm (High or Low): ")))
    (otherwise       (call-next-method))))



(defmethod update-prototype ((self quantity) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((formula)       #'(LAMBDA () (prompted-list-input "Formula: ")))
    ((importance)    #'(LAMBDA () (prompted-symbol-input "Importance (A-D): ")))
    ((norm)          #'(LAMBDA () (prompted-symbol-input "Norm (High or Low): ")))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (quantity-notes self))))
    (otherwise       (call-next-method))))


(defmethod no-print-readable-fields ((self quantity))
  (append '(sort-key instances isa-depth) (call-next-method)))

(defmethod table-index-fields ((self quantity))
  (append '(synonyms name keywords isa) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of quantities
;;----------------------------------------------------------

;; quantity is not defvar'd in db.lisp
(defvar quantity)
(setq quantity (make-db)) 
(setf (db-name quantity) 'quantity) 
(setf (db-prompt quantity) "quantity> ")
(setf (db-class quantity) 'quantity)
(setf (db-commands quantity)
      (append
       '((notes (notes note))
         (name (name))
         (isa (isa))
         (norm (norm))
         (english (english))
         (synonyms (synonyms synonym names))
         (formula (formula))
         (dimension (dimensions))
         )
       *record-db-commands*))

(init-db quantity)


;;----------------------------------------------------------
;;      Infer Norm
;;----------------------------------------------------------

(defun infer-norm (quant)
  (cond ((member quant '(high low)) quant)
        (t
         (if (atom quant)
           (let* ((q (if (quantity-p quant)
                       quant
                       (get-node quant quantity)))
                  (norm (slot-value q 'norm))
                  (form (slot-value q 'formula)))
             (cond (norm)
                   (form
                    (infer-norm-from-formula form))
                   (t nil)))
           (infer-norm-from-formula quant)))))


(defun infer-norm-from-formula (form)
  (cond ((atom form)
         (infer-norm form))
        (t
         (let ((left (car form))
               (operator (cadr form))
               (right (caddr form)))
           (qbc-arithmetic (infer-norm left) operator (infer-norm right))))))


(defun qbc-arithmetic (left operator right)
  (case operator
    ((* +) (qbc-plus left right))
    ((- /) (qbc-minus left right))
    (otherwise nil)))


(defun qbc-plus (left right)		
  (cond ((eq left right) left)
        ((numberp left) right)
        ((numberp right) left)
        ((prefer-more-important-one left right))
        (t  '*?*)))

;; this needs more work.
(defun qbc-minus (left right)		
  (cond	((numberp left) (qbc-opposite right))
        ((numberp right) left)
        ((prefer-more-important-one left right t))
        ((eq left right) '*?*)
        (t left)))

(defun qbc-opposite (side)
  (case side
    ((low) 'high)
    ((high) 'low)
    (otherwise nil)))


(defun prefer-more-important-one (left right &optional flip)
  (let* ((result (prefer-more-important left right))
         (ans (if result (preference-polarity result))))
    (cond ((and result flip)
           (qbc-opposite ans))
          (result
           ans)
          )))

(defun prefer-more-important (left right)
  (cond ((preference-p left)
         (if (preference-p right)
           (prefer-more-important2 left right)))
        ((preference-p right)
         right)
        (t nil)))

(defun prefer-more-important2 (left right)
  (cond ((>important? (preference-importance left)
                      (preference-importance right))
         left)
        ((<important? (preference-importance left)
                      (preference-importance right))
         right)
        (t nil)))




(defun qbc (n) (apply #'qbc-arithmetic n))

;; run test cases
;; --------------
(defun qbc-test ()
  (mapcar
   #'(lambda (n)
       (format t "~%~A => ~A" n (qbc n)))
   '((high + high)
     (high + low)
     (low + high)
     (low + low)
     (1 + high)
     (1 + low)
     
     (high - high)
     (high - low)
     (low - high)
     (low - low)
     (1 - high)
     (1 - low)
     
     (high * high)
     (high * low)
     (low * high)
     (low * low)
     (1 * high)
     (1 * low)
     
     (high / high)
     (high / low)
     (low / high)
     (low / low)
     
     (1 / high)
     (1 / low) ))
  '*end-of-qbc-test*)

