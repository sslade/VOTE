
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

(defclass member (record)
  ((name   :initform nil :documentation "name of member" :accessor member-name) 
   (fname  :initform nil :documentation "first name symbol for member" :accessor member-fname) 
   (lname  :initform nil :documentation "last name symbol for member" :accessor member-lname) 
   (english-short  :initform nil :documentation "short version of name" :accessor member-english-short)
   (notes   :initform nil :documentation "list of remarks" :accessor member-notes)
   (gender   :initform nil :documentation "male or female" :accessor member-gender)
   (votes   :initform nil :documentation "past voting record -- list of bill/vote pairs" :accessor member-votes)
   (new-votes :initform nil :documentation "test votes -- list of bill/vote pairs" :accessor member-new-votes)
   (stances   :initform nil :documentation "stances extracted from votes" :accessor member-stances)
   (issues   :initform nil :documentation "list of issues of importance to this member" :accessor member-issues)
   (credo   :initform nil :documentation "list of stances personal to this member" :accessor member-credo)
   (groups   :initform nil :documentation "list of groups of importance to this member" :accessor member-groups)
   (relations :initform nil :documentation "list of relations with groups" :accessor member-relations)
   (pro-rel-stances :initform nil :documentation "list of stances inferred from pro relationships" :accessor member-pro-rel-stances)
   (con-rel-stances :initform nil :documentation "list of stances inferred from con relationships" :accessor member-con-rel-stances)
   (stance-sort-key :initform nil :documentation "Symbol (LOYALTY or EQUITY) used for setting priorities" :accessor member-stance-sort-key)
   (district :initform nil :documentation "name of district from which elected" :accessor member-district)
   (term-start :initform nil :documentation "year elected to Congress" :accessor member-term-start)
   (term-end :initform nil :documentation "year left Congress" :accessor member-term-end)
   (party :initform nil :documentation "political party affiliation" :accessor member-party)
   (committees :initform nil :documentation "list of committees on which member serves" :accessor member-committees))
  (:documentation "Member Class"))

; (defpredicate member)

(defmethod all-the-slots ((obj member))
  (append '(name fname lname english-short notes gender votes new-votes stances issues 
            credo groups relations pro-rel-stances con-rel-stances stance-sort-key 
            district term-start term-end party committees)
          (call-next-method)))

(defmethod print-object ((self member) stream)
  (format stream "#{Member (~A) ~A}"
          (object-hash self) 
          (member-name self)))
   

(defmethod inorder? ((self member) (other member))
  (if (member? other)
    (let ((first (sort-key self))
          (second (sort-key other)))
      (string< first second))
    nil))


(defmethod sort-key ((self member))
    (if (or (null (member-fname self))
            (null (member-lname self)))
        (let ((name-list (filter (read-string->list (member-name self)) #'stop-table)))
            (setf (member-fname self) (car name-list))
            (setf (member-lname self) (car (last name-list)))))
    (concatenate 'string 
        (or (and (stringp (member-lname self)) (member-lname self))
            (string (member-lname self)))
        (or (and (stringp (member-fname self)) (member-fname self))
            (string (member-fname self)))))

(defmethod synonyms ((self member))
    (read-string->list (member-name self)))

(defmethod id ((self member))
  (cond ((null (record-symbol self))
         (setf (record-symbol self)
               (generate-symbol 'member)))
        (else (record-symbol self))))

(defmethod english-short ((self member))
  (cond ((member-english-short self))
        (else
         (let ((name-list (filter (read-string->list (member-name self)) #'stop-table)))
           (capitalize (->string (car (last name-list))))))))

   
(defmethod print-readable2 ((self member) stream)
  (format stream "~%(get-node ~A member)"
          (id self)))


(defmethod print-header ((self member) stream &rest args)
  (declare (ignore args))
  (format stream "~%~A ~A~A ~A ~10T~A~A ~40T(~A) ~A~53T(~A-~A)"
          (case (char (status self) 0)
            ((#\A #\O) #\space)
            ((#\P) #\/)
            (else  (char (status self) 0)))
          (if (current-flag self) "*" " ")
          (if (db-p (db self)) "db" "  ")
          (index self)
          (if (db-p (db self))
            (format nil "~S: " (db-name (db self)))
            "")
          (member-name self)
          (member-party self)
          (member-district self)
          (member-term-start self)
          (cond ((member-term-end self)) (else " "))))

(defmethod print-hc ((self member) stream &rest args)
  (format stream "~%~A ~30T(~A) ~A~42T(~A-~A)"
          (member-name self)
          (member-party self)
          (member-district self)
          (member-term-start self)
          (cond ((member-term-end self)) (else " ")))
  (mapc 
   #'(lambda (slot)
       (if (slot-value self slot)
         (progn 
           (format stream "~%~A: ~15T"
                   (capitalize slot))
           (pretty-print (slot-value self slot) stream)
           (terpri stream))))
   '(credo relations))
  (cond ((member `english (flatten args))
         (say-cluster-reasons self (member-credo self) stream)
         (say-cluster-relations self stream)
         ))
  (values)
  )
    

(defmethod pretty-print ((self member) stream)
  (call-next-method)
  (cond ((member-notes self)
         (format stream "~%Notes: ~15T********")
         (mapc
          #'(lambda (note)
              (pretty-print note stream))
          (member-notes self))))
 
  (say-cluster-reasons self (member-credo self) stream)
  (say-cluster-relations self stream)
  )


(defmethod input-prototype ((self member) slot)
  (case slot   
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((party)         #'(LAMBDA () (prompted-symbol-input "Party: ")))
    ((district)      #'(LAMBDA () (prompted-symbol-input "District: ")))
    ((votes)         #'(LAMBDA () (prompted-vote-input)))
    ((issues)        #'(LAMBDA () (prompted-list-input "Issues: ")))
    ((committees)    #'(LAMBDA () (prompted-list-input "Committees: ")))
    ((groups)        #'(LAMBDA () (prompted-list-input "Groups: ")))
    ((term-start)    #'(LAMBDA () (prompted-string-input "Year elected: ")))
    (otherwise       (call-next-method))))


(defmethod update-prototype ((self member) slot)
  (case slot
    ((name)          #'(LAMBDA () (prompted-string-input "Name: ")))
    ((english-short) #'(LAMBDA () (prompted-string-input "Short Name: ")))
    ((party)         #'(LAMBDA () (prompted-symbol-input "Party: ")))
    ((district)      #'(LAMBDA () (prompted-symbol-input "District: ")))
    ((fname)         #'(LAMBDA () (prompted-symbol-input "First name: ")))
    ((lname)         #'(LAMBDA () (prompted-symbol-input "Last name: ")))
    ((stance-sort-key) #'(LAMBDA () (prompted-symbol-input "Stance sort key: ")))
    ((gender)        #'(LAMBDA () (prompted-symbol-input "Gender: ")))
    ((votes)         #'(LAMBDA () (prompted-vote-input)))
    ((new-votes)     #'(LAMBDA () (prompted-vote-input)))
    ((stances)       #'(LAMBDA () (extract-voting-stances self)))
    ((pro-rel-stances)   #'(LAMBDA () (get-relations-stances self 'pro)
                            (member-pro-rel-stances self)))
    ((con-rel-stances)   #'(LAMBDA () (get-relations-stances self 'con)
                            (member-con-rel-stances self)))
    ((credo)         #'(LAMBDA () (prompted-stance-list-input "Credo stances: " (id self) 'member)))
    ((relations)     #'(LAMBDA () (prompted-relation-list-input "Relations: " (id self) 'member)))
    ((issues)        #'(LAMBDA () (prompted-list-input "Issues: ")))
    ((term-start)    #'(LAMBDA () (prompted-string-input "Year elected: ")))
    ((term-end)      #'(LAMBDA () (prompted-string-input "Year departed: ")))
    ((committees)    #'(LAMBDA () (prompted-list-input "Committees: ")))
    ((groups)        #'(LAMBDA () (prompted-list-input "Groups: ")))
    ((notes)         #'(LAMBDA () (cons (input-values (make-instance 'note))
                                        (member-notes self))))
    (otherwise       (call-next-method))))
 
(defmethod decision-dependencies ((self member))
  '(credo votes relations stance-sort-key))

(defmethod no-print-readable-fields ((self member))
  (append '(stances fname lname pro-rel-stances con-rel-stances) (call-next-method)))

(defmethod table-index-fields ((self member))
  (append '(name groups committees district) (call-next-method)))




;;----------------------------------------------------------
;;      Master Data Base of Members of Congress
;;----------------------------------------------------------

(setf member (make-db))
(setf (db-name member) 'member) 
(setf (db-prompt member) "MEMBER> ")
(SETF (db-class member) 'member)
(SETF (db-commands member)
      (append
       '((notes (notes note))
         (name (name))
         (english-short (short english-short))
         (fname (first fname))
         (lname (last lname))
         (votes (votes vote))
         (new-votes (new-votes))
         (party (party))
         (district (district))
         (stances (stances))
         (pro-rel-stances (rel-stances))
         ;;          (con-rel-stances (rel-stances))
         (stance-sort-key (stance-sort-key))
         (credo (credo))
         (gender (gender))
         (relations (rels relation relations))
         (term-start (term-start elected))
         (term-end (term-end departed))
         (issues (issues issue))
         (groups (group groups))
         (committees (committees committee)))
       *record-db-commands*)
       )

(init-db member)




(defun prompted-vote-input ()  
  (let* ((input (prompted-list-input "Voting record: [C, H98, H99, H100 or other] "))
         (votes
          (case (car input)
            ((C)   '(cap-gains-cut))
            ((h98) '(cap-tax-cut
                     extend-ss-benefit
                     estab-dom-content
                     bar-imm-amnesty
                     ok-school-pray
                     limit-abortions
                     approve-era
                     pass-imm-reform
                     cancel-mx-missile
                     halt-aid-to-contras
                     incr-aid-to-el-sal
                     supp-nuclear-freeze))
            ((h99) '(LMT-CLN-WATER-ACT
                     RPL-TOBAC-SUB 
                     GRM-RDMN-DEF-RED 
                     BAN-POLYGRAPH 
                     RETAIN-GUN-CONTROL 
                     CONTRA-AID 
                     LMT-TEXT-IMP 
                     LIMIT-SDI 
                     AID-ANGOLA-REB 
                     TAX-REFORM 
                     S-AFRICA-SANC 
                     IMMIG-REFORM))
            ((h100) '(defense-authorization
                       space-station
                       civil-rights-restoration-act
                       reflagging-ships
                       handgun-control
                       contra-aid2
                       star-wars
                       home-health-care
                       aids
                       limit-imports))
            (otherwise    input))))
    (mapcar 
     #'(lambda (vote)
;; check bill database for legal entries
         (get-node vote bill)
         (let* ((prompt (CONCATENATE 'STRING (STRING vote) ": "))
                (for/agn (prompted-symbol-input prompt)))
           (list vote
                 (case for/agn
                   ((y yes yea t f fo for) 'for)
                   ((n no nay agn a ag) 'agn)
                   (otherwise '--)))))
     votes)))

         


;;; For each vote of the member, extract the stances associated with
;;; that vote.
;;; 

(defun extract-voting-stances (member)  
  (my-format t "~%Extracting stances based on voting record of ~A..."
             (member-name member))  
  (let ((votes (member-votes member)))
    (prog1 
     (mappend #'extract-vote-stances votes)
     (my-format t "done.~%"))))

(defun extract-vote-stances (vote)  
  (let ((bill-id (car vote))
        (for-or-agn (cadr vote)))
    (let ((bill-node (get-node bill-id bill)))
      (case for-or-agn
        ((for) (bill-stance-for bill-node))
        ((agn) (bill-stance-agn bill-node))
        ((pro con) (format t "~%Expecting FOR or AGN. Not ~A in EXTRACT-STANCE"
                           for-or-agn)
         nil)
        (otherwise nil)))))

;; switched source field in stances for bills from bill-id to bill-number
;;  one time fix. 4/16/89
(defun fixit (b)  
  (let ((fors (bill-stance-for b))
        (agns (bill-stance-agn b))
        (num  (bill-bnumber b)))
    (setf (bill-stance-for b)
          (mapcar 
          #'(lambda (s)
            (setf (stance-source s) num)
            s)
          fors))
    (setf (bill-stance-agn b)
          (mapcar 
           #'(lambda (s)
               (setf (stance-source s) num)
               s)
           agns))
    ))



;;------------------------------------------------------------------
;;  Get the stances inferred from member's relations
;;------------------------------------------------------------------

(defun get-relations-stances (member side)
  (declare (ignore side))
  (let* ((rels (member-relations member))
         (stances
          (mappend
           #'(lambda (rel)
               (let* ((grp (get-node (relation-group rel) group))
                      (stances (group-stances grp)))
                 (mapcar 
                  #'(lambda (stance)
                      (setf (stance-relation stance) rel)
                      stance)
                  stances)))         
           rels))
         (pred #'(lambda (stance)
                 (eq  'pro (relation-side (stance-relation stance))))))
    (setf (member-con-rel-stances member)
         (filter stances pred))
    (setf (member-pro-rel-stances member)
          (collect stances pred))))

             
     
