

;; Congressman program


;;------------------------------------------------------------------
;;  Top-level 
;;------------------------------------------------------------------

(defun vote (member-id bill-id)  
  (let ((decision (make-instance 'decision))
        (old-line-length (line-length *standard-output*))
        (example-line-length 68))

    ;; implementation dependent -- forget it.
    ;; (setf (line-length *standard-output*) example-line-length)

    (if *no-output*
        (format t "."))

    (initialize-decision decision member-id bill-id)

    (update-decision-metrics decision)

    (apply-decision-strategies decision)

    (compare-with-real-vote decision)

    (update-decision-dbase decision)

    (out-switch
     (cond ((decision-strategy decision)
            (pretty-print decision *standard-output*))
           (else
            (indent-box so
                        "No decision"))))

    ;; (setf (line-length *standard-output*) old-line-length)

    decision

    ))



(defun update-decision-dbase (dec)  
    (my-format t "~%Adding current vote to DECISION database...")
    (insert-item (*db-table* 'decision) dec)
    (my-format t "done.~%"))


(defun hinge (port)  
  (my-format port "\\hinge{}"))

;;------------------------------------------------------------------
;;  Initialize decision structure
;;------------------------------------------------------------------

(defun initialize-decision (decision mem-id bill-id)  
  (let ((member-id (type-check+fix mem-id member 'vote))) 
    (cond ((member-stances member-id))
          (else
           (setf (member-stances member-id)
                (extract-voting-stances member-id))))
    
    (setf (decision-member decision) (copy member-id))

    (setf (decision-bill decision) (copy
                                   (type-check+fix bill-id bill 'vote)))

    (out-switch
     (indent-box so
                 (append 
                  (list (format nil "Decision: ~15T@~A" (object-hash decision))
                        (format nil "Member: ~15T~A" (member-name (decision-member decision)))
                        (format nil "Bill:~15T~A"    (bill-name (decision-bill decision))))
                  (remarks (decision-bill decision)))))

    (infer-rel-stances decision)
    
    (out-switch
     (center-box so "Analyzing alternative positions"))

    (setf (decision-for-stances decision) 
         (match-stances-for/agn decision 'for))
    
    (setf (decision-agn-stances decision) 
         (match-stances-for/agn decision 'agn))
    
    ))


;;------------------------------------------------------------------
;;  Infer stances from relations
;;------------------------------------------------------------------

(defun infer-rel-stances (decision)  
  (let ((mem-id (decision-member decision)))
    (infer-member-rel-stances mem-id)))

(defun infer-member-rel-stances (mem-id)  
  (cond ((null (member-pro-rel-stances mem-id))
         (my-format t "~%Inferring stances from relations of ~A..."
                 (member-name mem-id))
         (get-relations-stances mem-id 'pro)
         (my-format t "done.~%"))
        (else nil)))
                         


;;------------------------------------------------------------------
;;  Next-level 
;;------------------------------------------------------------------

(defun match-stances-for/agn (decision side)  
  (out-switch
   (progn   ;; 11/24/15
     (indent-box so (format nil "Vote ~A ~A"
			    side (bill-bnumber (decision-bill decision))))
     (format t "~%Considering implications of vote ~A ~A~&"
	     side (bill-bnumber (decision-bill decision)))
     (my-format t "Matching member stances with bill stances:~&")
     )
   )
  (let* ((bill-stance (if (eq side 'for)
                          #'bill-stance-for
                          #'bill-stance-agn))
         (sort-key (or (member-stance-sort-key (decision-member decision))
                       'equity))
         (stances (mappend
                   #'(lambda (stance)
                     (match-stances stance (decision-member decision)))
                   (funcall bill-stance (decision-bill decision)))))

    (my-format t "Sorting stances based on ~A order..." sort-key)
    (mapc 
     #'(lambda (stance)
       (set-sort-key stance sort-key))
     stances)
    (setf stances (msort (remove-old-votes stances (id (decision-bill decision)))))
    (my-format t "done.~%")
    (hinge so)
    (my-format t "Stances ~A: " side)
    (out-switch (my-pretty-print stances so))
    stances))


(defun remove-old-votes (stances billid)  
  (if *no-old-votes*
      (filter
       stances
       #'(lambda (st) (eq (id (reveal-source st))
                         billid)))
      stances))

(defun no-old-votes (flag)  
    (setf *no-old-votes* flag)
    flag)

(setf *no-old-votes* T)

;;; match-stances will check
;;; personal stances, voting record stances, group/relationship stances
;;;

(defun match-stances (stance-id mem-id)  
  (my-format t "~5T~A~&"
             stance-id)
  (let ((matches (mapcar 
                  #'(lambda (mem-stance)
                    (match? stance-id mem-stance))
                  (append (member-credo mem-id)
                          (member-stances mem-id)
                          (member-pro-rel-stances mem-id)))))

    (delete '() matches)))


;;------------------------------------------------------------------
;;  Apply decision strategies
;;------------------------------------------------------------------

(defun apply-decision-strategies (decision)  
  (out-switch (center-box so "Applying decision strategies"))
  (let ((strats (db-all strategy)))
    (apply-strats decision strats)))


(defun apply-strats (decision strats)  
  (cond ((null strats) nil)
        (else
         (let* ((strat (car strats))
                (code-name (strategy-test-code strat))
                (test (if code-name
                          (eval code-name)
                          nil)))
           (my-format t "~&Trying decision strategy: ~A..."
                      (strategy-name strat))
           (cond ((and test (funcall test decision strat))
                  (my-format t "success.~%")
                  (cond ((strategy-protocol strat)
                         (out-switch
                            ;; (hinge so)
                            ;;**((eval (strategy-protocol strat)) decision))))
			  (progn ;; 11/24/15
			    (apply (symbol-function (strategy-protocol strat)) (list decision))))))
			
                  (setf (decision-reason decision)
                       (group-reasons (decision-reason decision)))
                  (setf (decision-downside decision)
                       (group-reasons (decision-downside decision)))
                  decision)
                 (else
                  (my-format t "~A.~%"
                          (if test "failed"
                              "void"))
                  (apply-strats decision (cdr strats))))))))


;;------------------------------------------------------------------
;;  Group reasons
;;------------------------------------------------------------------

;;  
;;  Take the list of stances and group together the stances on the same
;;  issue
;;  

(defun group-reasons (stance-list)  
  (cond ((null stance-list) nil)
        (else
         (cons
          (collect  stance-list
                    #'(lambda (stance)
                      (match? (car stance-list) stance)))
          (group-reasons
           (filter stance-list
                   #'(lambda (stance)
                     (match? (car stance-list) stance))))))))

;;------------------------------------------------------------------
;;  Batch processing and analysis
;;------------------------------------------------------------------

;;  There are three ways of invoking VOTE-ALL
;;  
;;      (vote-all)               ;; process all members on all bills
;;      (vote-all 'member-name)  ;; process all bills for given member
;;      (vote-all 'bill-name)    ;; process all members for given bill
;;  
;;  Results will be put in decision dbase.
;;---------------------------------------------------------------------  
(defun vote-all (&rest opts)  
  (no-output t)
  (run-time
   (let* ((options (car opts))
          (billids (cond ((and options
                               (get-node options bill))
                          (list (get-node options bill)))
                         (else (db-all bill))))
          (memids (cond ((and options
                              (get-node options member))
                         (list (get-node options member)))
                        (else (db-all member)))))
     (mapc 
      #'(lambda (billid)
        (if (and *no-output*
                 (cddr billids)
                 (cddr memids))
            (format t "~%~a: " billid))
        (mapc 
         #'(lambda (memid)
           (vote memid billid))
         memids))
      billids)))
  (no-output nil))

