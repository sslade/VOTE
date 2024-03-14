
(defclass decision (record)
  ((isa-depth :initform "" :accessor decision-isa-depth)
   (sort-key  :initform nil :accessor decision-sort-key)
   (bill      :initform nil :accessor decision-bill)
   (member    :initform nil :accessor decision-member)
   (for-stances :initform nil :documentation "list of stances in favor" :accessor decision-for-stances)
   (agn-stances :initform nil :documentation "list of stances opposed" :accessor decision-agn-stances)
   
   (neg-for-stances :initform nil :documentation "list of flip stances in favor" 
                    :accessor decision-neg-for-stances)
   (neg-agn-stances :initform nil :documentation "list of flip stances opposed" 
                    :accessor decision-neg-agn-stances)
 
   (con-rel-for-stances :initform nil :documentation "list of stances in favor by opposition"
                        :accessor decision-con-rel-for-stances)
   (con-rel-agn-stances :initform nil :documentation "list of stances opposed by opposition"
                        :accessor decision-con-rel-agn-stances)

   (no-update :initform T :documentation "T until update-decision-metrics done"
              :accessor decision-no-update)
   (number-for :initform nil :documentation "number of reasons for" :accessor decision-number-for)
   (number-agn :initform nil :documentation "number of reasons agn" :accessor decision-number-agn)
 
   (group-for :initform nil :documentation "list of groups in support" :accessor decision-group-for)
   (group-agn :initform nil :documentation "list of groups opposed" :accessor decision-group-agn)

   (for-norms :initform nil :documentation "norms associated with for stances"
              :accessor decision-for-norms)
   (agn-norms :initform nil :documentation "norms associated with agn stances"
              :accessor decision-agn-norms) 

   (for-bnorms :initform nil :documentation "norms associated with bill's for stances"
              :accessor decision-for-bnorms)
   (agn-bnorms :initform nil :documentation "norms associated with bill's agn stances"
              :accessor decision-agn-bnorms)

   (split-group :initform nil :documentation "list of groups on both sides"
                :accessor decision-split-group)
   (split-record :initform nil :documentation "t/nil if voting record is split"
                :accessor decision-split-record)
   (split-credo :initform nil :documentation "t/nil if own credo is split"
                :accessor decision-split-credo)

   (MI-stance :initform nil :documentation "for/agn/nil -- (>important? for agn)"
              :accessor decision-MI-stance)
   (MI-group :initform nil :documentation "for/agn/nil" :accessor decision-MI-group)
   (MI-credo :initform nil :documentation "for/agn/nil" :accessor decision-MI-credo)
   (MI-record :initform nil :documentation "for/agn/nil" :accessor decision-MI-record)
   (MI-norm :initform nil :documentation "for/agn/nil" :accessor decision-MI-norm)
 
   (strategy :initform nil :documentation "decision strategy used to arrive at result"
             :accessor decision-strategy)
   (result :initform nil :documentation "for/agn -- position arrived at"
           :accessor decision-result)
   (reason  :initform nil :documentation "support for position" :accessor decision-reason)
   (downside :initform nil :documentation "negative aspects of decision" 
             :accessor decision-downside)
   (downside-record :initform nil :documentation "voting record stances supporting downside"
                    :accessor decision-downside-record)
;;;;  tradeoffs  ;; comparisons between good and bad aspects of decision
 
;;;;  source     ;; particular in decision of member, bill, issue, group
;;;;  importance
   
   (deeper-analysis :initform nil :documentation "set by deeper-analysis strategy"
                    :accessor decision-deeper-analysis)
   (real-vote :initform nil :documentation "actual vote for test bills"
              :accessor decision-real-vote)
   (score :initform nil :documentation "accuracy of predicted vote" :accessor decision-score))
  (:documentation "decision class definition"))


; (defpredicate decision)

(defmethod all-the-slots ((obj decision))
  (append '(isa-depth sort-key bill member for-stances agn-stances neg-for-stances 
            neg-agn-stances con-rel-for-stances con-rel-agn-stances no-update number-for 
            number-agn group-for group-agn for-norms agn-norms for-bnorms agn-bnorms 
            split-group split-record split-credo mi-stance mi-group mi-credo mi-record
            mi-norm strategy result reason downside downside-record deeper-analysis 
            real-vote score)
          (call-next-method)))


(defmethod pretty-print ((self decision) stream)
  (center-box so (format nil "Decision @~A" (object-hash self)))
  (call-next-method)
  (if (decision-result self) (english-rationale self stream))
  (center-box so (format nil "End of Decision @~A" (object-hash self))))


(defmethod print-object ((self decision) stream)
  (format stream "#{Decision (~A) [~A:~A] ~A (~A ~A ~A)~A}"
          (object-hash self)
          (decision-result self)
          (bill-bnumber (decision-bill self))
          (english-short (decision-member self))
          (member-party (decision-member self))
          (cond ((decision-score self))
                (else ":"))
          (member-district (decision-member self))
          (cond ((decision-deeper-analysis self))
                (else " "))
          ))


(defmethod print-hc ((self decision) stream &rest args)
    (declare (ignore args))
    (pretty-print self stream))


(defmethod inorder? ((self decision) (other decision))
  (if (decision-p other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))


(defmethod id ((self decision))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'decision)))
        (else (record-symbol self))))
   

(defmethod print-readable2 ((self decision) stream)
  (format stream "~%(get-node ~A decision)"
          (id self)))


(defmethod print-header ((self decision) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A~A  ~11T~A~A~A:~A ~24T~A~39T~A ~A ~A~50T~A ~A"
          (case (char (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db-p self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (decision-isa-depth self)
          (decision-result self)
          (bill-bnumber (decision-bill self))
          (english-short (decision-member self))
          (member-party (decision-member self))
          (if (decision-score self)
            (decision-score self)
            " ")
          (member-district (decision-member self))
          (cond ((decision-deeper-analysis self))
                (else " "))
          (strategy-name (decision-strategy self))))
   
(defmethod sort-key ((self decision))
  (cond ((decision-sort-key self))
        (else 
         (let ((s (concatenate 'string (sort-key (decision-bill self))
                          (sort-key (decision-member self)))))
           (setf (decision-sort-key self) s)
           s))))


(defmethod input-prototype ((self decision) slot)
  (case slot   
    ((isa-depth)     #'(lambda () ""))
    (otherwise       (call-next-method))))

(defmethod update-prototype ((self decision) slot)
  (case slot   
    ((no-update)     #'(LAMBDA () (update-decision-metrics self) nil))
    (otherwise       (call-next-method))))

 
(defmethod no-print-readable-fields ((self decision))
  (append '(sort-key isa-depth number-for 
            number-agn group-for group-agn for-bnorms agn-bnorms  
            for-norms agn-norms split-group split-record split-credo MI-stance MI-group  
            MI-credo MI-record MI-norm)
          (call-next-method)))



(defmethod table-index-fields ((self decision))
  (append '(bill member strategy) (call-next-method)))
     
 ;  ((table-index-fields self)
 ;   (list id
 ;         #'(lambda (item) (english-short (decision-bill item)))
 ;         #'(lambda (item) (english-short (decision-member item)))
 ;         #'(lambda (item) (strategy-name (decision-strategy item)))
 ;         ))
   
 

;;----------------------------------------------------------
;;      Master Data Base of Decisions
;;----------------------------------------------------------

(setf decision (make-db)) 
(setf (db-name decision) 'decision) 
(setf (db-prompt decision) "DECISION> ")
(setf (db-class decision) 'decision)
(setf (db-commands decision)
      (append
       '((no-update (update no-update)))  ;; !!
       *record-db-commands*)
      )
         

(init-db decision)


;;-------------------------------------------------------------------
;;  update decision metrics
;;-------------------------------------------------------------------

;;  number-for ;; number of reasons for
;;  number-agn ;; number of reasons agn
;;
;;  for-bnorms  ;; norms associated with bill's for stances
;;  agn-bnorms  ;; norms associated with bill's agn stances
;;
;;  for-norms  ;; norms associated with for stances
;;  agn-norms  ;; norms associated with agn stances
;;
;;  group-for  ;; list of groups in support
;;  group-agn  ;; list of groups opposed
;;
;;  split-group ;; list of groups on both sides
;;  split-record ;; t/nil if voting record is split
;;  split-credo ;; t/nil if own credo is split
;;
;;  MI-stance ;; for/agn/nil -- (>important? for agn)
;;  MI-group  ;; for/agn/nil
;;  MI-credo  ;; for/agn/nil
;;  MI-record ;; for/agn/nil
;;  MI-norm   ;; for/agn/nil
;;

(defun update-decision-metrics (decision)  
  (let ((fors (decision-for-stances decision))
        (agns (decision-agn-stances decision))
        (billid (decision-bill decision)))

    (setf (decision-number-for decision) (length fors))
    (setf (decision-number-agn decision) (length agns))

    (setf (decision-for-bnorms decision)  (check-norms (bill-stance-for billid)))
    (setf (decision-agn-bnorms decision)  (check-norms (bill-stance-agn billid)))

    (setf (decision-for-norms decision)  (check-norms fors))
    (setf (decision-agn-norms decision)  (check-norms agns))
    
    (setf (decision-group-for decision)
         (collect-groups fors))
    (setf (decision-group-agn decision)
         (collect-groups agns))

    (setf (decision-split-group decision)
         (intersection (decision-group-for decision)
                       (decision-group-agn decision)))
    (SETF (decision-split-record decision)
         (and (collect-bills fors)
              (collect-bills agns)))
    (setf (decision-split-credo decision)
         (and (collect-credo fors)
              (collect-credo agns)))

    (update-MI-stances decision)

    (setf (decision-no-update decision) nil)
    ))

(defun collect-groups (stances)  
  (collect-source-type 'group stances))

(defun collect-credo (stances)  
  (collect-source-type 'member stances))

(defun collect-bills (stances)  
  (collect-source-type 'bill stances))

(defun collect-source-type (db stances)  
  (collect
   stances
   #'(lambda (stance)
     (eq db (stance-source-db stance)))))

;;    MI-stance ;; for/agn/nil -- (>important? for agn)
;;    MI-group  ;; for/agn/nil
;;    MI-credo  ;; for/agn/nil
;;    MI-record ;; for/agn/nil
;;    MI-norm   ;; for/agn/nil

(defun update-MI-stances (decision)  
  (setf (decision-MI-stance decision)
       (MI-stance decision))
  (setf (decision-MI-group decision)
       (MI-stance decision 'group))
  (setf (decision-MI-credo decision)
       (MI-stance decision 'member))
  (setf (decision-MI-record decision)
       (MI-stance decision 'bill))
  (setf (decision-MI-norm decision)
       (compare-stances (decision-for-norms decision)
                        (decision-agn-norms decision)))
  )
  

(defun MI-stance (decision &rest db)  
  (let ((fors (decision-for-stances decision))
        (agns (decision-agn-stances decision)))
    (let ((db-type (car db)))
      (cond (db-type
             (setf fors (collect-source-type db-type fors))
             (setf agns (collect-source-type db-type agns))))
      (compare-stances fors agns))))



(defun compare-stances (fors agns)  
  (cond ((and (null fors)
              (null agns))
         nil)
        ((null fors)
         (list 'agn (remove-less-important agns)))
        ((null agns)
         (list 'for (remove-less-important fors)))
        ((>important? (car fors) (car agns))
         (list 'for (remove-less-important fors)))
        ((<important? (car fors) (car agns))
         (list 'agn (remove-less-important agns)))
        (else
         (compare-stances (cdr fors) (cdr agns)))))

;;  Takes a list of sorted stances and removes all stances less
;;  important than the first one in the list.

(defun remove-less-important (stances)  
  (filter
   stances
   #'(lambda (stance)                   ; 
     (<important? stance (car stances)))))


(defun check-norms (stances)  
  (let ((norms (mapcar #'normative-stance? stances)))
    (if (member nil norms)
        nil
        (remove-duplicates norms))))
         
;; predicate for checking if a given stance is consistent with
;; the normative position on that issue

(defun normative-stance? (stance)  
  (let ((norm (issue-norm (get-node (stance-issue stance) issue))))
    (if norm
        (match? stance norm)
        nil)))


