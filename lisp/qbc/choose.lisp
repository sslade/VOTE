(defun choose (agent1 option1 option2)
  (if (not (agent-p agent1))
    (setf agent1 (get-node agent1 agent)))
  (if (not (option-p option1))
    (setf option1 (get-node option1 option)))
  (if (not (option-p option2))
    (setf option2 (get-node option2 option)))
  
  (let* ((c (make-instance 'choice)))
    (setf (choice-agent c) agent1)
    (setf (choice-option1 c) option1)
    (setf (choice-option2 c) option2)
    
    (format t "~A must choose between ~A and ~A~%"
            (agent-name agent1)
            (option-name option1)
            (option-name option2))
    
    (process-data c (option-data option1))
    
    c
    ))


		
(defun process-data (choice data1)
  (cond ((null data1))
        (t
         (compare-data choice (car data1))
         (process-data choice (cdr data1)))))


(defun compare-data (choice datum1)
  (let* ((quant (datum-issue datum1))
         (datum2 (find-if #'(lambda (d) (eq quant (datum-issue d)))
                          (option-data (choice-option2 choice))))
         (pref (get-preference (choice-agent choice) quant))
	 )
    (format t "~%Option 1: ~A" datum1)
    (format t "~%Option 2: ~A" datum2)
    (format t "~%Preference: ~A" pref)
    (case (preference-polarity pref)
      ((high) (choose-high choice datum1 datum2))
      ((low) (choose-low choice datum1 datum2))
      (otherwise nil))
    ))

(defun choose-high (choice datum1 datum2)
  (let ((val1 (datum-value datum1))
        (val2 (datum-value datum2)))
    (cond ((> val1 val2)
           (choose-option1 choice datum1 'high))
          ((> val2 val1)
           (choose-option2 choice datum2 'high))
          (t
           (choose-equal choice)))))

(defun choose-low (choice datum1 datum2)
  (let ((val1 (datum-value datum1))
        (val2 (datum-value datum2)))
    (cond ((< val1 val2)
           (choose-option1 choice datum1 'low))
          ((< val2 val1)
           (choose-option2 choice datum2 'low))
          (t
           (choose-equal choice)))))

(defun choose-option1 (choice datum polarity)
  (push datum (choice-option1-data choice))
  (state-preference 1 polarity))

(defun choose-option2 (choice datum polarity)
  (push datum (choice-option2-data choice))
  (state-preference 2 polarity))

(defun choose-equal (choice)
  (state-preference 0 'equal))

(defun state-preference (num polarity)
  (case polarity
    ((high) (format t "~%Prefer option ~A, which is higher." num))
    ((low) (format t "~%Prefer option ~A, which is lower." num))
    ((equal) (format t "~%Indifferent between options."))))

