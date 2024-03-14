
;;  Like VOTE-ALL, ANALYZE-SCORES has optional arguments
;;  
;;  Prints out statistics about decision dbase.
;;  (analyze-decisions 'deep) uses only deep-analysis decisions
;;--------------------------------------------------------------------- 
(defun analyze-scores (&rest opts)  
  (let* ((options (car opts))
         (memids (mapcar 
                  #'id
                  (cond ((and options
                              (get-node options member))
                         (list (get-node options member)))
                        (else (db-all member)))))
         (strats (mapcar #'id (collect (db-all strategy) #'strategy-test-code)))
         (stats  '(for+ agn+ for- agn-))
         (deep-levels '(a b c d))
         (decisions (cond ((and options
                                (member options '(deep)))
                           (collect (db-all decision) #'decision-deeper-analysis))
                          (else
                           (collect (db-all decision) #'decision-score))))
         (billids (remove-duplicates
                   (mapcar 
                    #'id
                    (cond ((and options
                                (get-node options bill))
                           (list (get-node options bill)))
                          (else (mapcar #'decision-bill decisions))))))
         (indent 11)
         (tabbing 3))

    ;; initialize stuff to zero
    (format t "~%Initializing...")
    (mapc 
     #'(lambda (dec-id)
       (let* ((strat (id (decision-strategy dec-id)))
              (level (decision-deeper-analysis dec-id))
              (side (decision-result dec-id))
              (score (case (decision-score dec-id)
                       ((+) '+)
                       ((-) '-)
                       (else nil)))
              (billid (id (decision-bill dec-id)))
              (memid (id (decision-member dec-id)))
              (stat (->symbol (format nil "~A~A" side score))))
         (if score
             (mapc 
              #'(lambda (node)
                (setf (get node stat) 0))
              (list billid 'bill-total memid 'member-total strat 'strat-total)))
                
         (format t ".")))
     decisions)

    (format t "~%Processing decisions")
    (mapc 
     #'(lambda (dec-id)
       (let* ((strat (id (decision-strategy dec-id)))
              (level (decision-deeper-analysis dec-id))
              (side (decision-result dec-id))
              (score (case (decision-score dec-id)
                       ((+) '+)
                       ((-) '-)
                       (else nil)))
              (billid (id (decision-bill dec-id)))
              (memid (id (decision-member dec-id)))
              (stat (->symbol (format nil "~A~A" side score))))
         (if score
             (mapc 
              #'(lambda (node)
                (get+1 node stat))
              (list billid 'bill-total memid 'member-total strat 'strat-total)))
                
         (format t ".")))
     decisions)
    (format t "~%")
    
    (print-score-results opts)

    ))


(defun print-score-results (&rest opts)  
  (let* ((options (car opts))
         (billids (remove-duplicates
                   (mapcar 
                    #'id
                    (cond ((and options
                                (get-node options bill))
                           (list (get-node options bill)))
                          (else (mapcar #'decision-bill (db-all decision)))))))
         (memids (mapcar 
                  #'id
                  (cond ((and options
                              (get-node options member))
                         (list (get-node options member)))
                        (else (db-all member)))))
         (stats  '(for+ agn+ for- agn-))
         (top-stats (append '(rgt% wrg%) stats))
         (strats (mapcar #'id (collect (db-all strategy) #'strategy-test-code)))
         (deep-levels '(a b c))  ;; ignore level D
         
         (grand-total (apply #'+
                             (mapcar #'(lambda (stat) (cond ((get 'bill-total stat))
                                                       (else 0)))
                                  stats)))
         (indent 25)
         (tabbing 6))
  

    ;; Print results.

    ;; print aggregate scores

    (print-aggregate-scores billids indent tabbing)

    ;; print bill results
    (format t "~%~%Score Results:~%")
    (print-one-line-scores "Bill" top-stats indent tabbing *standard-output*)
    (terpri *standard-output*)


    (mapc 
     #'(lambda (billid)
       (print-one-line-scores (bill-bnumber (get-node billid bill))
                              (right-wrong
                               (mapcar #'(lambda (stat) (get billid stat))
                                    stats))
                              indent
                              tabbing
                              *standard-output*))
     billids)

    ;; print bill totals
    (print-one-line-scores "Bill Totals."
                           (right-wrong (mapcar 
                                         #'(lambda (stat) (get 'bill-total stat))
                                         stats))
                           indent tabbing *standard-output*)

    ;; print members
    (terpri *standard-output*)
    (print-one-line-scores "Member." top-stats indent tabbing *standard-output*)
    (terpri *standard-output*)

    (mapc 
     #'(lambda (memid)
       (print-one-line-scores (capitalize (member-lname (eval memid)))
                              (right-wrong
                               (mapcar #'(lambda (stat) (get memid stat))
                                    stats))
                              indent
                              tabbing
                              *standard-output*))
     memids)

    ;; print member totals
    (print-one-line-scores "Member Totals."
                           (right-wrong
                            (mapcar 
                             #'(lambda (stat) (get 'member-total stat))
                             stats))
                           indent tabbing *standard-output*)

    ;; print strategys
    (terpri *standard-output*)
    (print-one-line-scores "Strategy & Number" top-stats indent tabbing *STANDARD-OUTPUT*)
    (terpri *standard-output*)

    (mapc 
     #'(lambda (strat)
       (print-one-line-scores (concatenate 'string 
                               (strategy-name (eval strat))
                               (format nil "& ~A"
                                       (index (eval strat))))
                              (right-wrong
                               (mapcar #'(lambda (stat) (get strat stat))
                                    stats))
                              indent
                              tabbing
                              *standard-output*))
     strats)

    ;; print strategy totals
    (print-one-line-scores "Strategy Totals & "
                           (right-wrong
                            (mapcar 
                             #'(lambda (stat) (get 'strategy-total stat))
                             stats))
                           indent tabbing *standard-output*)


    (format t "~%Total decisions: ~A.~%" grand-total)

    (values)
    
    ))

(defun right-wrong (args)  
  (let* ((nargs (mapcar #'->number args))
         (for+ (first nargs))
         (agn+ (second nargs))
         (for- (third nargs))
         (agn- (fourth nargs))
         (right (+ for+ agn+))
         (wrong (+ for- agn-))
         (total (+ right wrong))
         (right% (percentage right total))
         (wrong% (percentage wrong total)))
    (list right% wrong% for+ agn+ for- agn-)))

(defun ->number (a)  
  (if (numberp a)
      a
      0))    
  

(defun print-one-line-scores (string lst indent tabbing port)  
  (format port "~A" string)
  (tab-to-column indent port)
  (tabular-print port lst tabbing))
  


(defun get+1 (node prop)  
  (cond ((get node prop)
         (incf (get node prop)))
        (else
         (setf (get node prop) 1))))


;;  calculate whether each aggregate vote matches original outcome.

(defun print-aggregate-scores (billids indent tabbing)  
  (print-one-line-scores "Bills"
                         '("FOR" "AGN" "DIF" "Pred" "Real" "Right/Wrong")
                         indent
                         tabbing
                         *standard-output*)

  (init-aggregate-total)

  (mapc 
   #'(lambda (billid)
     (print-one-aggregate-score (id (get-node billid bill)) indent tabbing))
   billids)

  (let* ((right (get 'aggregate-total 'right))
         (wrong (get 'aggregate-total 'wrong))
         (na    (get 'aggregate-total 'na))
         (total (safe+ right wrong)))
    (format t "~%Aggregate correct: ~A out of ~A.  ~A percent.~%"
            right total (percentage right total))
    (if (> na 0)
        (format t "~A hypothetical votes not included.~%"
                na))))

(defun safe+ (a b)  
  (+ (if (numberp a)
         a
         0)
     (if (numberp b)
         b
         0)))


(defun init-aggregate-total ()
  (setf (get 'aggregate-total 'na) 0)
  (setf (get 'aggregate-total 'right) 0)
  (setf (get 'aggregate-total 'wrong) 0)
  )

(defun update-aggregate-total (outcome)  
  (get+1 'aggregate-total outcome))

(defun print-one-aggregate-score (billid indent tabbing)  
  (let* ((real-outcome (get-real-outcome billid))
         (fors (safe+ (get billid 'for+)
                      (get billid 'for-)))
         (agns (safe+ (get billid 'agn+)
                      (get billid 'agn-)))
         (predicted-outcome (get-predicted-outcome billid))
         (outcome (cond ((member real-outcome '(for agn))
                         (if (eq real-outcome predicted-outcome)
                             'RIGHT
                             'WRONG))
                        (else "NA")))
         (port *standard-output*))
    (update-aggregate-total outcome)
    (print-one-line-scores (bill-bnumber (get-node billid bill))
                           (list fors agns (abs (- fors agns))
                                 predicted-outcome real-outcome outcome)
                           indent tabbing port)
    ))


(defun get-real-outcome (billid)  
  (let* ((tally (bill-vote-tally (get-node billid bill))))
    (cond ((null tally) nil)
          (else
           (case (car tally)
             ((passed adopted approved) 'for)
             ((failed rejected) 'agn)
             (else nil))))))

(defun get-predicted-outcome (billid)  
  (let ((fors (safe+ (get billid 'for+)
                     (get billid 'for-)))
        (agns (safe+ (get billid 'agn+)
                     (get billid 'agn-)))
        (factor (cond ((bill-majority-factor (get-node billid bill)))
                      (else 1))))
    (if (>= fors (safe* factor agns))
        'for
        'agn)))

(defun safe* (a b)  
  (cond ((and (numberp a) (numberp b))
         (* a b))
        (else 0)))


(defun analyze-all ()
  (analyze-decisions)
  (tally-scores)
  (tally-da-scores)
  (analyze-scores)
  (analyze-decisions 'deep)
  (setf *current-db* strategy)
  (headers_command)
 ;; (group-check-*)  ;; test code
  (setf *current-db* decision)
  (headers_command)
)
