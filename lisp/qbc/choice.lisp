
(defclass choice (record)
  ((isa-depth :initform "" :accessor choice-isa-depth)
   (sort-key  :initform nil :accessor choice-sort-key)
   (option1   :initform nil :accessor choice-option1)
   (option2   :initform nil :accessor choice-option2)
   (agent    :initform nil :accessor choice-agent)
   (option1-data :initform nil :documentation "list of stances in favor" :accessor choice-option1-data)
   (option2-data :initform nil :documentation "list of stances opposed" :accessor choice-option2-data)
   
   (neg-option1-data :initform nil :documentation "list of flip stances in favor" 
                    :accessor choice-neg-option1-data)
   (neg-option2-data :initform nil :documentation "list of flip stances opposed" 
                    :accessor choice-neg-option2-data)
 

   (number-for :initform nil :documentation "number of reasons for" :accessor choice-number-for)
   (number-agn :initform nil :documentation "number of reasons agn" :accessor choice-number-agn)
 
   (for-norms :initform nil :documentation "norms associated with for stances"
              :accessor choice-for-norms)
   (agn-norms :initform nil :documentation "norms associated with agn stances"
              :accessor choice-agn-norms) 

   (strategy :initform nil :documentation "choice strategy used to arrive at result"
             :accessor choice-strategy)
   (result :initform nil :documentation "for/agn -- position arrived at"
           :accessor choice-result)
   (reason  :initform nil :documentation "support for position" :accessor choice-reason)
   (downside :initform nil :documentation "negative aspects of choice" 
             :accessor choice-downside)
   (downside-record :initform nil :documentation "voting record stances supporting downside"
                    :accessor choice-downside-record)
;;;;  tradeoffs  ;; comparisons between good and bad aspects of choice
 
;;;;  source     ;; particular in choice of agent, option1, issue, group
;;;;  importance
   

   )
  (:documentation "choice class definition"))


(defpredicate choice)

(defmethod all-the-slots ((obj choice))
  (append '(isa-depth sort-key option1 option2 agent option1-data
					  option2-data neg-option1-data
					  neg-option2-data   number-for 
            number-agn for-norms agn-norms 
			strategy result reason downside downside-record 
			)
          (call-next-method)))


(defmethod pretty-print ((self choice) stream)
  (center-box so (format nil "Choice @~A" (object-hash self)))
  (call-next-method)
  (if (choice-result self) (english-rationale self stream))
  (center-box so (format nil "End of Choice @~A" (object-hash self))))


(defmethod print-object ((self choice) stream)
  (format stream "\\verb|#{Choice (~A) [~A:~A] ~A}|"
          (object-hash self)
          (option-name (choice-option1 self))
		  (option-name (choice-option2 self))
          (english-short (choice-agent self))
          ))


(defmethod print-hc ((self choice) stream &rest args)
    (declare (ignore args))
    (pretty-print self stream))


(defmethod inorder? ((self choice) (other choice))
  (if (choice-p other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))


(defmethod id ((self choice))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'choice)))
        (else (record-symbol self))))


(defmethod print-readable2 ((self choice) stream)
  (format stream "~%(get-node ~A choice)"
          (id self)))


(defmethod print-header ((self choice) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A~A  ~11T~A~A~A:~A ~24T~A"
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
          (choice-isa-depth self)
          (choice-result self)
          (option-name (choice-option1 self))
          (english-short (choice-agent self))
		  ))

(defmethod sort-key ((self choice))
  (cond ((choice-sort-key self))
        (else 
         (let ((s (concatenate 'string (sort-key (choice-option1 self))
                          (sort-key (choice-agent self)))))
           (setf (choice-sort-key self) s)
           s))))


(defmethod input-prototype ((self choice) slot)
  (case slot   
    ((isa-depth)     #'(lambda () ""))
    (otherwise       (call-next-method))))

(defmethod update-prototype ((self choice) slot)
  (case slot   
    (otherwise       (call-next-method))))

 
(defmethod no-print-readable-fields ((self choice))
  (append '(sort-key isa-depth number-for 
            number-agn 
            for-norms agn-norms)
          (call-next-method)))



(defmethod table-index-fields ((self choice))
  (append '(option1 agent strategy) (call-next-method)))
     
 ;  ((table-index-fields self)
 ;   (list id
 ;         #'(lambda (item) (english-short (choice-option1 item)))
 ;         #'(lambda (item) (english-short (choice-agent item)))
 ;         #'(lambda (item) (strategy-name (choice-strategy item)))
 ;         ))
   
 

;;----------------------------------------------------------
;;      Master Data Base of Choices
;;----------------------------------------------------------

(defvar choice)
(setf choice (make-db)) 
(setf (db-name choice) 'choice) 
(setf (db-prompt choice) "CHOICE> ")
(setf (db-class choice) 'choice)
(setf (db-commands choice)
       *record-db-commands*)

         

(init-db choice)

