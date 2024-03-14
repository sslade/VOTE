

;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------
(defclass group (record)
  ((name   :initform nil :documentation "name of group" :accessor group-name) 
   (notes   :initform nil :documentation "list of remarks" :accessor group-notes)
   (gender   :initform nil :documentation "male or female" :accessor group-gender)
   (issues   :initform nil :documentation "list of issues of importance to this group" :accessor group-issues)
   (sort-key :initform nil :documentation "sort key" :accessor group-sort-key)
 
   (stances   :initform nil :documentation "list of stances" :accessor group-stances)
   (credo   :initform nil :documentation "list of stances personal to this group" :accessor group-credo)


   (english  :initform nil :documentation "English version of group name" :accessor group-english)
   (english-short :initform nil :documentation "short English phrase for group" 
                  :accessor group-english-short)
   (pro-english  :initform nil :documentation "English phrase biased favorably" :accessor group-pro-english)
   (con-english  :initform nil :documentation "pejorative English phrase" :accessor group-con-english)
   (french   :initform nil :documentation "French version of group name" :accessor group-french)
   (french-short :initform nil :documentation "short French phrase for group" 
                  :accessor group-french-short)
   (pro-french  :initform nil :documentation "French phrase biased favorably" :accessor group-pro-french)
   (con-french  :initform nil :documentation "pejorative French phrase" :accessor group-con-french)
   (number      :initform nil :documentation "sing or plur for generation" :accessor group-number)
   (synonyms      :initform nil :documentation "List of symbols" :accessor group-synonyms)
   (pro-stances :initform nil :documentation "List of stances supporting this group" 
                :accessor group-pro-stances)
   (con-stances :initform nil :documentation "List of stances opposing this group" 
                :accessor group-con-stances)
   (norm        :initform nil :documentation "Normative relation to this group" :accessor group-norm)
   (isa       :initform nil :documentation "List of symbols" :accessor group-isa)
   (isa-depth  :initform nil :documentation "String for padding printing" :accessor group-isa-depth)
   (instances  :initform nil :documentation "List of groups" :accessor group-instances))
   
  (:documentation "group Class"))

; (defpredicate group)

(defmethod all-the-slots ((obj group))
  (append '(name notes gender issues sort-key stances credo english english-short 
            pro-english con-english french french-short pro-french con-french number 
            synonyms pro-stances con-stances norm isa isa-depth instances)
          (call-next-method)))

(defmethod print-object ((self group) stream)
  (format stream "#{Group (~A) ~A}"
          (object-hash self) 
          (group-name self)))
   

(defmethod english-short ((self group))
  (cond (*english*
         (group-english-short self))
        (else
         (group-french-short self))))


(defmethod english ((self group))
  (cond (*english*
         (if (group-english self)
           (group-english self)
           (concatenate 'string "**" (group-name self) "**")))
        ((group-french self))
        (else
         (concatenate 'string "**" (group-name self) "**"))))


(defmethod inorder? ((self group) (other group))
  (if (group? other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))


(defmethod synonyms ((self group))
  (let ((name (read-string->list (group-name self))))
    (if (= (length name) 1)
      (append name (group-synonyms self))
      (group-synonyms self))))

(defmethod sort-key ((self group))
  (if (group-sort-key self)
    (group-sort-key self)
    (group-name self)))


(defmethod set-isa-sort ((self group))
  (setf (group-isa-depth self) (isa-depth self group))
  (setf (group-sort-key self) (generate-isa-sort-key self group)))


(defmethod set-alpha-sort ((self group))
  (setf (group-isa-depth self) "")
  (setf (group-sort-key self) (group-name self)))


(defmethod id ((self group))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'group)))
        (else (record-symbol self))))


   
(defmethod print-readable2 ((self group) stream)
  (format stream "~%(get-node ~A group)"
          (id self)))



(defmethod print-header ((self group) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~A~A~50T~A"
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
          (if (group-isa-depth self)
            (group-isa-depth self)
            "")
          (group-name self)
          (cond ((group-synonyms self))
                (else ""))))

   


(defmethod print-hc ((self group) stream &rest args)
  (if (or (group-stances self)
          (member `all (flatten args)))
    (progn 
      (format stream "~%~A~20T~A~%"
              (group-name self)
              (if (group-english self)
                (group-english self)
                ""))
      (mapc 
       #'(lambda (slot)
           (if (slot-value self slot)
             (progn 
               (format stream "~%~A: ~15T"
                       (capitalize slot))
               (pretty-print (slot-value self slot) stream)
               (terpri stream))))
       '(pro-english con-english norm stances))
     
      (cond ((and (member `english (flatten args))
                  (or (group-norm self)
                      (group-stances self)))
             (if (group-norm self)
               (paragraph (norm-test self) stream))
             (say-cluster-reasons self (group-stances self) stream)
             ))
      (values)
      )))


(defmethod pretty-print ((self group) stream)
  (call-next-method)
  (cond ((group-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (group-notes self))))
 
  (say-cluster-reasons self (group-stances self) stream)
  (paragraph (norm-test self) stream)
  )


(defmethod input-prototype ((self group) slot)
  (case slot   
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    (otherwise       (call-next-method))))




(defmethod update-prototype ((self group) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english)       #'(LAMBDA () (prompted-string-input "English: ")))
    ((english-short) #'(LAMBDA () (prompted-string-input "Short English Phrase: ")))
    ((pro-english)   #'(LAMBDA () (prompted-string-input "Pro English phrase: ")))
    ((con-english)   #'(LAMBDA () (prompted-string-input "Con English phrase: ")))
    ((french)        #'(LAMBDA () (prompted-string-input "French: ")))
    ((french-short)  #'(LAMBDA () (prompted-string-input "Short French Phrase: ")))
    ((pro-french)    #'(LAMBDA () (prompted-string-input "Pro French phrase: ")))
    ((con-french)    #'(LAMBDA () (prompted-string-input "Con French phrase: ")))
    ((isa)           #'(LAMBDA () (prompted-list-input "ISA: ")))
    ((issues)        #'(LAMBDA () (prompted-list-input "Issues: ")))
    ((number)        #'(LAMBDA () (prompted-symbol-input "Number (sing or plur): ")))
    ((gender)        #'(LAMBDA () (prompted-symbol-input "Gender: ")))
    ((stances)       #'(LAMBDA () (prompted-stance-list-input "Stances: " (car (synonyms self)) 'group)))
    ((norm)          #'(LAMBDA () (format t "~%Normative stance for this group:~%")
                        (input-relation (id self) 'group (car (synonyms self)))))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (group-notes self))))
    (otherwise       (call-next-method))))
 
(defmethod decision-dependencies ((self group))
  '(stances norm))

(defmethod no-print-readable-fields ((self group))
  (append '(instances sort-key isa-depth) (call-next-method)))

(defmethod table-index-fields ((self group))
  (append '(name synonyms isa issues) (call-next-method)))




;;----------------------------------------------------------
;;      Master Data Base of Groups
;;----------------------------------------------------------

(setf group (make-db))
(setf (db-name group) 'group) 
(setf (db-prompt group) "GROUP> ")
(setf (db-class group) 'group)
(setf (db-commands group)
      (append
       '((notes (notes note))
         (name (name))
         (english-short (short english-short))
         (english (english eng))
         (pro-english (pro-english pro-e))
         (con-english (con-english con-e))
         (french (french))
         (french-short (french-short))
         (pro-french (pro-french pro-f))
         (con-french (con-french con-f))
         (norm (norm norms))
         (synonyms (synonyms synonym names))
         (stance-sort-key (stance-sort-key))
         (keywords (keywords keys key))
         (isa (isa))
         (number (number))
         (gender (gender))
         (issues (issues issue))
         (stances (stances stance))
         )
       *record-db-commands*)
       )

       
 
(init-db group)


;; switched source field in stances for bills from bill-id to bill-number
;;  one time fix. 4/16/89
(defun gfixit (grp)  
  (let ((stances (group-stances grp))
        (name  (car (synonyms grp))))
    (setf (group-stances grp)
         (mapcar 
          #'(lambda (s)
            (setf (stance-source s) name)
            s)
          stances))
    ))

