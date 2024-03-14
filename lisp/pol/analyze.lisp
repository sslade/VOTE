
;;  Like VOTE-ALL, ANALYZE-DECISIONS has optional arguments
;;  
;;  Prints out statistics about decision dbase.
;;  (analyze-decisions 'deep) uses only deep-analysis decisions
;;---------------------------------------------------------------------          
(defun analyze-decisions (&rest opts)  
  (let* ((options (car opts))
         (billids (mapcar 
                   #'id
                   (cond ((and options
                               (get-node options bill))
                          (list (get-node options bill)))
                         (else (db-all bill)))))
         (memids (mapcar 
                  #'id
                  (cond ((and options
                              (get-node options member))
                         (list (get-node options member)))
                        (else (db-all member)))))
         (strats (collect (db-all strategy) #'strategy-test-code))
         (deep-levels '(a b c d))
         (deep? (and options (member options '(deep))))
         (decisions (cond (deep?
                           (collect (db-all decision) #'decision-deeper-analysis))
                          (else
                           (db-all decision))))
         (indent 11)
         (tabbing 3))

    ;; initialize property lists for FOR/AGN tallies
    (setf (get 'bill-total 'for) 0)
    (setf (get 'member-total 'for) 0)
    (setf (get 'bill-total 'agn) 0)
    (setf (get 'member-total 'agn) 0)
    (mapc 
     #'(lambda (itemid)
       (setf (get itemid 'for) 0)
       (setf (get itemid 'agn) 0))
     (append memids billids))

    ;; initialize property lists for strategies    
    (mapc 
     #'(lambda (strat)
       (setf (get 'bill-total strat) 0)
       (setf (get 'member-total strat) 0)
       (mapc 
        #'(lambda (billid)
          (setf (get billid strat) 0))
        billids)
       (mapc 
        #'(lambda (memid)
          (setf (get memid strat) 0))
        memids))
     strats)
    
    ;; initialize property lists for deep analysis levels   
    (mapc 
     #'(lambda (level)
       (mapc 
        #'(lambda (billid)
          (setf (get billid level) 0)
          (setf (get 'bill-total level) 0))
        billids)
       (mapc 
        #'(lambda (memid)
          (setf (get memid level) 0)
          (setf (get 'member-total level) 0))
        memids))
     deep-levels)
    
    ;; count apportioned strats
    (format t "~%Processing decisions")
    (mapc 
     #'(lambda (dec-id)
       (let ((strat (decision-strategy dec-id))
             (level (decision-deeper-analysis dec-id))
             (side (decision-result dec-id))
             (billid (id (decision-bill dec-id)))
             (memid (id (decision-member dec-id))))
         (get+1 billid strat)
         (get+1 'bill-total strat) 
         (get+1 memid strat)
         (get+1 'member-total strat) 
         ;; tally FOR/AGN
         (get+1 billid side)
         (get+1 'bill-total side) 
         (get+1 memid side)
         (get+1 'member-total side) 
         (cond (level
                (get+1 billid level)
                (get+1 'bill-total level) 
                (get+1 memid level)
                (get+1 'member-total level)))
                
         (format t ".")))
     decisions)

    (print-results deep?)

    ))


(defun print-results (deep?)  
  (let* ((options deep?)
         (billids (mapcar #'id (db-all bill)))
         (memids (mapcar #'id (db-all member)))
         (strats (filter (collect (db-all strategy) #'strategy-test-code)
                         #'(lambda (strat) (zerop (get 'bill-total strat)))))
         (deep-levels (if deep?
                          '(a b c FOR AGN)  ;; ignore level D
                          '()))
         (strat-list (append (mapcar #'index strats) deep-levels))
         (new-strats (append strats deep-levels))
         (decisions (db-all decision))
         (grand-total (apply #'+
                             (mapcar #'(lambda (strat) (get 'bill-total strat))
                                  strats)))
         (indent 11)
         (tabbing 3))
  

    ;; Print results.
    (format t "~%~%Results:~%")
    (print-one-line "Bill/Strategy." strat-list indent tabbing *standard-output*)
    (terpri *standard-output*)

    (mapc 
     #'(lambda (billid)
       (print-one-line (bill-bnumber (get-node billid bill))
                       (mapcar #'(lambda (strat) (get billid strat))
                            new-strats)
                       indent
                       tabbing
                       *standard-output*))
     billids)

    ;; print bill totals
    (print-one-line "Bill Totals." (mapcar 
                                    #'(lambda (strat) (get 'bill-total strat))
                                    new-strats)
                    indent tabbing *standard-output*)

    ;; print members
    (terpri *standard-output*)
    (print-one-line "Member/Strategy." strat-list indent tabbing *STANDARD-OUTPUT*)
    (terpri *standard-output*)

    (mapc 
     #'(lambda (memid)
       (print-one-line (capitalize (member-lname (eval memid)))
                       (mapcar #'(lambda (strat) (get memid strat))
                            new-strats)
                       indent
                       tabbing
                       *standard-output*))
     memids)

    ;; print member totals
    (print-one-line "Member Totals." (mapcar 
                                    #'(lambda (strat) (get 'member-total strat))
                                    new-strats)
                    indent tabbing *standard-output*)

    (format t "~%Total decisions: ~A.~%" grand-total)

    (values)
    
    ))



(defun print-one-line (string lst indent tabbing port)  
  (format port "~A" string)
  (tab-to-column indent port)
  (tabular-print port lst tabbing))
  

;; procedures to compare predicted vote with real vote

(defun compare-with-real-vote (decision)  
  (if (set-real-vote decision)
      (set-score decision)
      'not-a-test-vote))

(defun set-real-vote (decision)  
  (let ((memid (decision-member decision))
        (billid (decision-bill decision)))
    (let ((names (synonyms billid))
          (votes (member-votes memid)))
      (setf (decision-real-vote decision)
           (find-real-vote names votes)))))


(defun find-real-vote (names votes)  
  (cond ((null votes) nil)
        ((intersection names (car votes))
         (cadar votes))
        (else
         (find-real-vote names (cdr votes)))))


(defun set-score (decision)  
  (let ((result (decision-result decision))
        (real-vote (decision-real-vote decision)))
    (cond ((null real-vote) nil)
          (else
           (setf (decision-score decision)
                (cond ((eq result real-vote) '+)
                      ((member real-vote '(for agn)) '-)
                      (else '?)))))))


(defun tally-scores ()
  (let ((correct-for 0)
        (correct-agn 0)
        (wrong-for 0)
        (wrong-agn 0)
        )
    (mapc 
     #'(lambda (decision)
       (case (decision-score decision)
         ((+) (case (decision-result decision)
                ((for) (setf correct-for (1+ correct-for)))
                ((agn) (setf correct-agn (1+ correct-agn)))))
         ((-) (case (decision-result decision)
                ((for) (setf wrong-for (1+ wrong-for)))
                ((agn) (setf wrong-agn (1+ wrong-agn)))))
         (otherwise nil)))
     (db-all decision))
    (print-scores correct-for correct-agn wrong-for wrong-agn)))

(defun print-scores (correct-for correct-agn wrong-for wrong-agn)  
  (let* ((correct (+ correct-for correct-agn))
         (wrong (+ wrong-for wrong-agn))
         (total (+ correct wrong))) 
    (format t "~30TFor~35TAgn
Correct:      ~D (~D%)~30T~D~35T~D
Wrong:        ~D (~D%)~30T~D~35T~D
Total:        ~D
"
            correct (percentage correct total) correct-for correct-agn
            wrong   (percentage wrong total)   wrong-for wrong-agn
            total)
    (values)))

(defun percentage (a b)  
  (if (zerop b)
      0
      (truncate (* 100 (/ a b)))))


(defun tally-da-scores ()
  (let ((correct-for 0)
        (correct-agn 0)
        (wrong-for 0)
        (wrong-agn 0)
        )
    (mapc 
     #'(lambda (decision)
       (if (decision-deeper-analysis decision)
           (case (decision-score decision)
             ((+) (case (decision-result decision)
                    ((for) (setf correct-for (1+ correct-for)))
                    ((agn) (setf correct-agn (1+ correct-agn)))))
             ((-) (case (decision-result decision)
                    ((for) (setf wrong-for (1+ wrong-for)))
                    ((agn) (setf wrong-agn (1+ wrong-agn)))))
             (otherwise nil))))
     (db-all decision))
    (format t "~%Deeper Analysis Score Totals~%")
    (print-scores correct-for correct-agn wrong-for wrong-agn)))
