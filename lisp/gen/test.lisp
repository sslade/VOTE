

(defun gen-test (source db iss)  
  (let* ((source-id (get-node source db))
         (iss-id    (get-node iss issue))
         (sides     '(pro con))
         (importances '(a b c)))
    
    (MAPC 
     #'(LAMBDA (side)
       (MAPC 
        #'(LAMBDA (importance)
          (let ((stance (init-stance (list iss-id side importance source-id db))))
            (init-context)
            (format t "~%Side: ~A.  Importance: ~A.~%  ~A"
                    side importance (eng-stance source-id stance))))
        importances))
     sides)
    (VALUES)))
    

(defun gen-record-test (memb iss)  
  (let* ((source-id (get-node memb member))
         (iss-id    (get-node iss issue))
         (np        (gen-record-np source-id))
         (sides     '(pro con))
         (importances '(a b c)))
    
    (MAPC 
     #'(LAMBDA (side)
       (MAPC 
        #'(LAMBDA (importance)
          (let ((stance (init-stance (list iss-id side importance source-id bill))))
            (init-context)
            (format t "~%Side: ~A.  Importance: ~A.~%  ~A"
                    side importance (eng-stance np stance))))
        importances))
     sides)
    (VALUES)))

(defun gen-test-np (item db case)  
  (repeat 6 (format t "~A~%" 
                    (if db
                        (gen-np (get-node item db) case)
                        (gen-np item case))))
  (VALUES))





(defun gen-test-no-cluster (subj stances)  
  (let ((new-subj (coerce-subj subj)))
    (paragraph
     (MAPCAR #'(LAMBDA (st) (say new-subj st))
          stances)
     so)))

(defun gen-test-cluster (subj stances)  
  (let ((new-subj (coerce-subj subj)))
    (say-cluster-reasons new-subj (msort stances) so)))


(defun coerce-subj (subj)  
  (let* ((item (car subj))
         (db (cadr subj))
         (realitem (get-node item db)))
    (cond ((issue-p realitem)
           (proto-dnp issue-np
                      'number 'sing
                      'issue t
                      'lex (CONCATENATE 'STRING "support of " (gen-np realitem 'obj))))
          (else
           realitem))))
