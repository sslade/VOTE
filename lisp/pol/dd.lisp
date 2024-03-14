
;;;;    Decision data dependency structure


(defclass dd ()
  ((updates    :initform nil 
               :documentation "slot+item pairs that have been changed, e.g.(#{Selector ISSUE NORM} . #{Issue (634) B-1 Bomber})"
               :accessor dd-updates)
   (old-updates :initform nil
                :documentation "slot+item pairs that have been processed"
                :accessor dd-old-updates)
   (decisions  :initform nil
               :documentation "decisions that might be affected by updates"
               :accessor dd-decisions)
   (old-decisions :initform nil
                  :documentation "old decisions ?"
                  :accessor dd-old-decisions)
   (changed-strategy :initform nil
                     :documentation "pairs old/new decisions that have same result, but new strat"
                     :accessor dd-changed-strategy)
   (changed-result :initform nil
                   :documentation "old/new pairs that have new result"
                   :accessor dd-changed-result))
  (:documentation "decision data dependency class"))

(defmethod all-the-slots ((obj dd))
  '(updates old-updates decisions old-decisions changed-strategy changed-result))
 

(defmethod print-object ((self dd) stream)
  (format stream "#{Decision data dependency (~A)}"
          (object-hash self)))


(defmethod pretty-print ((self dd) stream)
  (print-object self stream)
  (mapc 
   #'(lambda (slot)
       (cond ((null (slot-value self slot))
              nil)
             
             ((member slot (no-pp-fields self))
              nil)

             (else
              (format stream "~%~A: ~15T~A"
                      (capitalize slot)
                      (slot-value self slot)))))
   (all-the-slots self)))

(defmethod no-pp-fields ((self dd))
   '(old-updates old-decisions))


(setf *ddd* (make-instance 'dd)) 
;; original version of push-update in ../db.t
;; this one assumes both DECISION database and *ddd*
;;------------------------------------------------------
(defun push-update (item selector db &rest old-db)  
  (let ((update (list item db (car old-db)))
        (root-db  (find-root-db db)))
    (if (and (db-all decision)
             (memq selector (decision-dependencies item)))
        (push (cons selector item) (dd-updates *ddd*)))
    (if (member update (db-update-list root-db) :test #'equalp)
        (push nil (db-update-list root-db))
        (push update (db-update-list root-db)))))


(defun process-dd-updates (&rest dd)  
  (cond ((null dd)
         (setf dd *ddd*))
        (else
         (setf dd (car dd))))
  (cond ((dd-updates dd)
         (my-format t "~%Checking decision implications of updates ")
         (mapc 
          #'(lambda (update)
              (process-one-dd-update update dd))
          (dd-updates dd))
         (push (dd-updates dd) (dd-old-updates dd))
         (setf (dd-updates dd) '())
         (print-dd-results dd))
        (else
         (my-format t "~%No data dependency updates: ~A"
                    dd)))
  (values))

(defun process-one-dd-update (update dd)  
  (let ((decision-list (db-all (*db-table* 'decision))))
    (my-format t "~%Processing update: ~A~%" update)
    (mapc 
     #'(lambda (decision)
       (check-one-decision decision update dd))
     decision-list)))

(defun check-one-decision (decision update dd)  
  (let ((slot (car update))
        (item (cdr update)))
    (cond ((case slot
             ;; issue
             ((pro-stances con-stances)
              (check-decision-deeper-analysis item slot decision))
             ((norm)
              (progn 
                ;;(format t "~%Checking ISSUE-NORM update...")
                (memq item
                      (mapcar #'reveal-issue
                              (append (bill-stance-for (decision-bill decision))
                                      (bill-stance-agn (decision-bill decision)))))))
             ;; bill
             ((stance-for stance-agn importance)
              (progn 
                ;;(format t "~%Checking BILL-~A update..." (selector-id selector))
                (eq (id item)
                    (id (decision-bill decision)))))
             ;; member
             ((credo votes relations stance-sort-key)
              (progn 
                ;;(format t "~%Checking MEMBER-~A update..." slot)
                (eq (id item)
                    (id (decision-member decision)))))
             ;; group
             ((stances)
              ;;(format t "~%Checking GROUP-~A update..." slot)
                    (member item (mapcar #'reveal-group
                                     (member-relations (decision-member decision)))))
             (otherwise nil))
;;;;           (format t "~%   Change in ~A field of item: ~A ~%   may affect decision: ~A"
;;;;                   (selector-id selector)
;;;;                   item decision)
           (my-format t "*")               
           (if (memq decision (dd-decisions dd))
               nil
               (push decision (dd-decisions dd) )))
          (else 
           (my-format t ".")               
           ;; (format t "OK.~%")
           nil))))
                   
(defun check-decision-deeper-analysis (iss-id slot dec-id)  
  (let* ((side (case slot
                 ((pro-stances) 'pro)
                 ((con-stances) 'con)))
         (level (decision-deeper-analysis dec-id))
         (stances (append (bill-stance-for (decision-bill dec-id))
                          (bill-stance-agn (decision-bill dec-id))))
         (target (init-stance (list iss-id side 'b 'issue 'issue))))
    (cond ((null level) nil)
          ((collect stances
                    #'(lambda (stance) (match? target stance))))
          ((eq level `a) nil)
          (else
           (check-decision-deeper-level target stances nil level)))))

(defun check-decision-deeper-level (target stances level anal-level)  
  (let* ((next-level (next-analysis-level level))
         (new-stances (expand-stances stances next-level)))
    (cond ((collect #'(lambda (stance) (match? target stance))
                    new-stances))
          ((eq next-level anal-level)
           nil)
          (else
           (check-decision-deeper-level target new-stances next-level anal-level)))))

    
       

;;  
;;      issue-norm issue-pro-stances issue-con-stances
;;      group-stances  ?? group-norm
;;      bill-importance
;;      bill-stance-for
;;      bill-stance-agn
;;      member-credo
;;      member-votes (member-stances)
;;      member-relations (member-pro-rel-stances member-con-rel-stances)
;;  


(defun print-dd-results (dd)  
  (let ((decisions (dd-decisions dd)))
    (cond (decisions
           (my-format t "~%Votes that may be affected by changes:~%")
           (mapc 
            #'(lambda (dec)
              (let ((mem (decision-member dec))
                    (bil (decision-bill dec)))
                (my-format t "~A ~10T~A~%"
                           (english-short bil)
                           (member-name mem))))
            decisions)
           (values))
          (else nil))))


(defun process-dd-decisions (&rest dd)  
  (run-time
   (cond ((null dd)
          (setf dd *ddd*))
         (else
          (setf dd (car dd))))
   (let ((decisions (dd-decisions dd)))
     (cond ((null decisions) nil)
           (else
            (mapc 
             #'(lambda (decision)
               (process-one-dd-decision decision dd))
             decisions)
            (push (dd-decisions dd) (dd-old-decisions dd))
            (setf (dd-decisions dd) '())
            'end-of-process-dd-decisions)))))


(defun process-one-dd-decision (dec-id dd)  
  (no-output t)
  (let* ((memid (decision-member dec-id))
         (billid (decision-bill dec-id))
         (new-dec (vote (sanitize-member memid) billid))
         (old-strat (decision-strategy dec-id))
         (new-strat (decision-strategy new-dec))
         (old-result (decision-result dec-id))
         (new-result (decision-result new-dec)))
    (no-output nil)
    (cond ((not (eq old-result new-result))
           (my-format t "~%New result for vote: ~A on bill: ~A~%"
                      (english-short memid)
                      (english-short billid))
           (push (cons dec-id new-dec) (dd-changed-result dd)
                 ))
          ((not (eq old-strat new-strat))
           (my-format t "~%New strategy for vote: ~A on bill: ~A~%"
                      (english-short memid)
                      (english-short billid))
           (push (cons dec-id new-dec) (dd-changed-strategy dd)
                 ))
          (else nil))))


;; reset stance fields to nil so that they will be inferred from scratch
;; in revising the vote.
(defun sanitize-member (memid)  
  (setf (member-pro-rel-stances memid) nil)
  (setf (member-con-rel-stances memid) nil)
  (setf (member-stances memid) nil)
  memid)
         
    
