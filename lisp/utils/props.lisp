;;
;;  Property lists
;;          (dps . l)
;;          (dpsq . l)    [syntax]
;;          (isa-get node prop)
;;          (ddput node prop val)
;;          (*put node prop val)
;;          (add-property node prop val)
;;          (enter lst val)
;;          (ppp node)
;;          (pppq node)   [syntax]

;! dps    Define PropertieS function
;     with some smarts;  data driven by flags on the properties;
;     might be better to rewrite it in terms of objects;
;   (dps item property1 value1 property2 value2 ...)
;;  see T book, page 390
;-----------------------------------------------------------------------

(defun dps (&rest l)  
  (let ((node (car l)))
    (do ((l (cdr l) (cddr l)))
        ((null l) nil)
        (ddput node (car l) (cadr l)))))


(DEFMACRO dpsq (&rest l)  
  `(APPLY #'dps ',l))

(defun isa-get (node prop)  
  (cond ((get node prop))
        ((get node 'isa)
         (isa-get (get node 'isa) prop))
        (else nil)))

;;  ddput flags:
;;  "data-driven put"
;;      !invert-property
;;          (put prop node val)
;;          (put prop val node)
;;      !invert-value
;;          (put val prop node)
;;          (put val node prop)
;;      !multiple-values
;;          pushes value on list
;;      !push-prop
;;          when (ddput obj node prop val)
;;              if (get obj node prop) =>
;;              then (ppush obj prop X)     ??
;;      (ppush obj node prop val)
;;          (cond ((get obj node prop)
;;                 =>
;;                 (lambda (x) (push x val))))
;;      dd-props
;;          property for storing properties.
;;      lambda-prop
;;          procedure to be executed

(defun ddput (node prop val)  
  (and
   (SYMBOLP node)
   (SYMBOLP prop)
   (PROGN 
    (cond ((isa-get prop '!invert-property)
           (ddput prop val node)))
    (cond ((isa-get prop '!invert-value)
           (*put val prop node)))
    (let ((onto (isa-get prop '!invert-onto)))
      (if onto (ddput val onto node)))
    (cond ((isa-get prop '!multiple-values)
           (add-property node prop val))
          ((and (get node prop)
                (isa-get prop '!save-property))
           (*put (or (get node 'save-node)
                     (ddput node 'save-node (gensym 'save)))
                 prop
                 (get node prop))
           (*put node prop val))
          (else
           (let ((fn (isa-get prop '!lambda-property)))
             (if fn
                 (APPLY #'fn (list node prop val))
               (*put node prop val)))))
    val)))

(defun *put (node prop val)  
  (setf (get node '!ddprops) (enter (get node '!ddprops) prop))
  (setf (get node prop) val))

(defun add-property (node prop val)  
  (*put node prop (enter (get node prop) val)))

;; enter (from uci-lisp)
;-----------------------------------------------------------------------
(defun enter (list value)
  (adjoin value list))


(defun ppp (node)  
  (format t "~%~A" node)
  (MAPCAR #'(LAMBDA (prop)
                    (format t "~%~4T~A ~22T~A" prop (get node prop)))
          (get node '!ddprops))
  (VALUES))

(DEFMACRO pppq (node)  
  `(ppp ',node))

