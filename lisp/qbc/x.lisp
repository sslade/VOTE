
(defun get-preference (agt quant)
  (if (not (agent-p agt))
    (setf agt (get-node agt agent)))
  (if (not (quantity-p quant))
    (setf quant (get-node quant quantity)))
  (let ((prefs (agent-preferences agt)))
    (cond ((find-if #'(lambda (p) (eq quant (get-node (preference-issue p) quantity))) prefs))
          (t
           (infer-preference-from-formula agt quant)))))

(defun get-preference-polarity (agt quant)
  (preference-polarity (get-preference agt quant)))

(defun get-preference-importance (agt quant)
  (preference-importance (get-preference agt quant)))


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


;; this is from qbc.lisp

(defun qbc-arithmetic (left operator right)
  (case operator
    ((* +) (qbc-plus left right))
    ((- /) (qbc-minus left right))
    (otherwise nil)))


(defun qbc-plus (left right)		
  (cond ((eq left right) left)
        ((numberp left) right)
        ((numberp right) left)
        ((prefer-more-important left right))
        (t  '*?*)))

;; this needs more work.
(defun qbc-minus (left right)		
  (cond	((numberp left) (qbc-opposite right))
        ((numberp right) left)
        ((prefer-more-important left right))
        ((eq left right) '*?*)
        (t left)))

(defun qbc-opposite (side)
  (case side
    ((low) 'high)
    ((high) 'low)
    (otherwise nil)))

;; added 5/28/96
(defun qbc-same-polarity (left right)
  (let ((lval (qbc-get-polarity left))
        (rval (qbc-get-polarity right)))
    (eq lval rval)))

; added 5/28/96
(defun qbc-get-polarity (pref)
  (if (preference-p pref)
    (preference-polarity pref)
    pref))

(defun prefer-more-important (left right)
  (cond ((preference-p left)
         (if (preference-p right)
           (prefer-more-important2 left right)))
        ((preference-p right)
         right)
        (t nil)))

(defun prefer-more-important2 (left right)
  (cond ((prefer-more-important? left right)
         left)
        ((prefer-more-important? right left)
         right)
        (t nil)))


(defun prefer-more-important? (left right)
  (>important? (preference-importance left)
               (preference-importance right)))

