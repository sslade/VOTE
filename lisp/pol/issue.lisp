
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

(defclass issue (record)
  ((name   :initform nil :documentation "name of issue" :accessor issue-name) 
   (type   :initform nil :documentation "type of issue" :accessor issue-type) 
   (sort-key :initform nil :documentation "issue Sort Key" :accessor issue-sort-key)
   (synonyms :initform nil :documentation "issue synonyms - list of symbols" :accessor issue-synonyms)
   (isa       :initform nil :documentation "List of symbols" :accessor issue-isa)
   (isa-depth  :initform nil :documentation "String for padding printing" :accessor issue-isa-depth)
   (instances  :initform nil :documentation "List of issues" :accessor issue-instances)
   (polarity  :initform nil :documentation "Intrinsic position for this version of issue" 
              :accessor issue-polarity)
   (english  :initform nil :documentation "English version of issue name" :accessor issue-english)
   (english-short :initform nil :documentation "short English phrase for issue" 
                  :accessor issue-english-short)
   (pro-english  :initform nil :documentation "English phrase biased favorably" :accessor issue-pro-english)
   (con-english  :initform nil :documentation "pejorative English phrase" :accessor issue-con-english)
   (french   :initform nil :documentation "French version of issue name" :accessor issue-french)
   (french-short :initform nil :documentation "short French phrase for issue" 
                  :accessor issue-french-short)
   (pro-french  :initform nil :documentation "French phrase biased favorably" :accessor issue-pro-french)
   (con-french  :initform nil :documentation "pejorative French phrase" :accessor issue-con-french)
   (number  :initform nil :documentation "sing or plur for generation" :accessor issue-number)
   (groups    :initform nil :documentation "List of symbols" :accessor issue-groups)
   (pro-stances :initform nil :documentation "List of stances supporting this issue" 
                :accessor issue-pro-stances)
   (con-stances :initform nil :documentation "List of stances opposing this issue" 
                :accessor issue-con-stances)
   (norm        :initform nil :documentation "Normative stance on this issue" :accessor issue-norm)
   (opposite     :initform nil :documentation "issue opposite of current issue" 
                 :accessor issue-opposite)
   (notes     :initform nil :documentation "List of remarks" :accessor issue-notes))
  (:documentation "issue Class"))

; (defpredicate issue)

(defmethod all-the-slots ((obj issue))
  (append '(name type sort-key synonyms isa isa-depth
            instances polarity english english-short pro-english con-english
            french french-short pro-french con-french number groups pro-stances con-stances 
            norm opposite notes)
          (call-next-method)))


(defmethod print-object ((self issue) stream)
  (format stream "#{Issue (~A) ~A}"
          (object-hash self) 
          (issue-name self)))
   
(defmethod english ((self issue))
  (cond (*english*
         (if (issue-english self)
           (issue-english self)
           (concatenate 'string (issue-name self) " (*)")))
        (else
         (if (issue-french self)
           (issue-french self)
           (concatenate 'string "**" (issue-name self) "**")))))

(defmethod english-short ((self issue))
  (if *english*
    (issue-english-short self)
    (issue-french-short self)))

(defmethod inorder? ((self issue) (other issue))
  (if (issue? other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))

(defmethod synonyms ((self issue))
  (let ((name (read-string->list (issue-name self))))
    (if (= (length name) 1)
      (append name (issue-synonyms self))
      (issue-synonyms self))))

(defmethod sort-key ((self issue))
  (if (issue-sort-key self)
    (issue-sort-key self)
    (issue-name self)))
;;        (block 
;;          (set (issue-isa-depth self) (isa-depth self issue))
;;          (set (issue-sort-key self) (generate-isa-sort-key self issue)))))

(defmethod set-isa-sort ((self issue))
  (setf (issue-isa-depth self) (isa-depth self 'issue))
  (setf (issue-sort-key self) (generate-isa-sort-key self 'issue))
    )

(defmethod set-alpha-sort ((self issue))
  (setf (issue-isa-depth self) "")
  (setf (issue-sort-key self) (issue-name self)))

(defmethod id ((self issue))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'issue)))
        (else (record-symbol self))))
   
(defmethod print-readable2 ((self issue) stream)
  (format stream "~%(get-node ~A issue)"
          (id self)))

(defmethod print-header ((self issue) stream &rest args)
  (declare (ignore args)) 
  (format stream "~%~A ~A~A ~A ~10T~A~A~A~40T~A~50T~A~A"
          (case (CHAR (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (otherwise  (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (if (issue-isa-depth self)
            (issue-isa-depth self)
            "")
          (issue-name self)
          (if (issue-type self)
            (issue-type self)
            "")
          (let ((norm (issue-norm self)))
            (if norm
              (format nil "~A~A"
                      (case (stance-side norm)
                        ((pro) "+")
                        ((con) "-"))
                      (stance-importance norm))
              "  "))
          (cond ((issue-synonyms self))
                (else ""))))
   
(defmethod print-hc ((self issue) stream &rest args)
;;  (declare (ignore args))
  (if (or (issue-norm self)
          (issue-pro-stances self)
          (issue-con-stances self))
    (progn 
      (format stream "~%~A~26T~A"
              (issue-name self)
              (if (issue-english self)
                (issue-english self)
                ""))
      (mapc
       #'(lambda (slot)
           (if (slot-value self slot)
             (progn 
               (format stream "~%~A:~26T"
                       (capitalize slot))
               (my-pretty-print (slot-value self slot) stream)
               )))
       '(opposite norm pro-stances con-stances))

;;    (format t "ARGS: ~A~%" args)
      (cond ((member (car (flatten args)) '(long full english))
             (paragraph (norm-test self) stream)
             (say-p/c self stream)
             )
            (else
             (format stream "~&")))
      (values)
      )))


(defmethod pretty-print ((self issue) stream)
  (call-next-method)
  (cond ((issue-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (issue-notes self))))
  (paragraph (norm-test self) stream)
  (say-p/c self stream)
  )

(defmethod input-prototype ((self issue) slot)
  (case slot
    ((symbol)        #'(LAMBDA () (generate-symbol 'issue)))
    ((isa-depth)     #'(LAMBDA () ""))
    ((status)        #'(LAMBDA () "Active"))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((type)          #'(LAMBDA () (prompted-symbol-input "Type: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English phrase: ")))
    ((polarity)      #'(LAMBDA () (prompted-string-input "Polarity: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((date-open)     #'(LAMBDA () (current_date)))
    (otherwise       (call-next-method))))

(defmethod update-prototype ((self issue) slot)
  (case slot
    ((status)        #'(LAMBDA () (capitalize (prompted-string-input "Status: "))))
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((type)          #'(LAMBDA () (prompted-symbol-input "Type: ")))
    ((remarks)       #'(LAMBDA () (prompted-string-list-input "Remarks: ")))
    ((date-open)     #'(LAMBDA () (prompted-date-input "Date-open: ")))
    ((date-closed)   #'(LAMBDA () (prompted-date-input "Date-closed: ")))
    ((db)            #'(LAMBDA () (prompted-string-input "Db: ")))
    ((keywords)      #'(LAMBDA () (prompted-list-input "Keywords: ")))
    ((synonyms)      #'(LAMBDA () (prompted-list-input "Synonyms: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((groups)        #'(LAMBDA () (prompted-list-input "Groups: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English phrase: ")))
    ((english-short) #'(LAMBDA () (prompted-string-input "Short English phrase: ")))
    ((pro-english)   #'(LAMBDA () (prompted-string-input "Pro English phrase: ")))
    ((con-english)   #'(LAMBDA () (prompted-string-input "Con English phrase: ")))
    ((french)        #'(LAMBDA () (prompted-string-input "French phrase: ")))
    ((french-short)  #'(LAMBDA () (prompted-string-input "Short French phrase: ")))
    ((pro-french)    #'(LAMBDA () (prompted-string-input "Pro French phrase: ")))
    ((con-french)    #'(LAMBDA () (prompted-string-input "Con French phrase: ")))
    ((number)        #'(LAMBDA () (prompted-symbol-input "Number (sing or plur): ")))
    ((opposite)      #'(LAMBDA () (prompted-issue-input "Opposite issue: " self)))
    ((polarity)      #'(LAMBDA () (prompted-string-input "Polarity: ")))
    ((norm)          #'(LAMBDA () (format t "~%Normative stance for this issue:~%")
                        (input-stance (id self) 'issue (car (synonyms self)))))
    ((pro-stances)   #'(LAMBDA () (prompted-stance-list-input "Issue PRO stances: " (car (synonyms self)) 'issue)))
    ((con-stances)   #'(LAMBDA () (prompted-stance-list-input "Issue CON stances: " (car (synonyms self)) 'issue)))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (issue-notes self))))
    (otherwise       (call-next-method))))
 
  

(defmethod decision-dependencies ((self issue))
    '(norm pro-stances con-stances))

(defmethod get-stances ((self issue) side)
  (case side
    ((pro) (issue-pro-stances self))
    ((con) (issue-con-stances self))
    (otherwise nil)))

(defmethod no-print-readable-fields ((self issue))
  (append '(sort-key instances isa-depth) (call-next-method)))

(defmethod table-index-fields ((self issue))
  (append '(synonyms symbol english-short type isa keywords groups) (call-next-method)))


;;----------------------------------------------------------
;;      Master Data Base of Issues
;;----------------------------------------------------------

(setq issue (make-db)) 
(SETF (db-name issue) 'issue) 
(SETF (db-prompt issue) "ISSUE> ")
(SETF (db-class issue) 'issue)
(SETF (db-commands issue)
        '(
          (notes (notes note))
          (name (name))
          (synonyms (synonyms synonym names))
          (type (type))
          (isa (isa))
          (groups (groups group))
          (opposite (opposite))
          (norm (norm norms))
          (pro-stances (pro-stances pro-s))
          (con-stances (con-stances con-s))
          (english (english eng))
          (english-short (english-short short))
          (pro-english (pro-english pro-e))
          (con-english (con-english con-e))
          (french (french))
          (french-short (french-short))
          (pro-french (pro-french pro-f))
          (con-french (con-french con-f))
          (number (number))
          (polarity (polarity polar))
          ))

(init-db issue)

(defun prompted-issue-input (prompt source-iss)  
  (let* ((input (prompted-symbol-input prompt))
         (iss (get-node input issue)))
    (if iss
        (progn 
          (setf (issue-opposite iss) (car (synonyms source-iss)))
          (push-update iss 'update-field *current-db*)))
    input))

           
