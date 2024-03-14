
(defun english-rationale (decision stream)  

    (indent-box stream "English rationale:")

    (init-context)   ;; initialize context for generation

    (preamble decision stream)

    (paragraph
     (mapcar 
      #'(lambda (reason)
        (if (listp reason)
            (setq reason (car (flatten reason))))
        (format nil "~A"
                (say (decision-member decision) reason)))
      (decision-reason decision))
     stream)

    (cond ((decision-downside decision)
           (let ((intro (gen-downside-intro decision))
                 (reason-list
                  (mapcar 
                   #'(lambda (reason)
                     (if (listp reason)
                         (setq reason (car (combine-stances (flatten reason)))))
                     (let ((speaker (case (stance-source-db reason)
                                      ((bill)        *record*)
                                      ((member)      (decision-member decision))
                                      ((issue)       (gen-norm-np))
                                      ((group)       (if (stance-siblings reason)
                                                         (create-sibling-np reason)
                                                         (reveal-source reason)))
                                      (otherwise     (gen-norm-np)))))
                       (format nil "~A"
                               (say speaker reason))))
                   (decision-downside decision))))
             (paragraph
              (cons (concatenate 'string intro " " (car reason-list))
                    (cdr reason-list))
              stream))))

    (cond ((decision-downside-record decision)
           (let* ((intro (gen-downside-record-intro decision))
                  (speaker (gen-record-np (decision-member decision)))
                  (reason-list
                   (mapcar 
                    #'(lambda (reasons)
                      (format nil "~A"
                              (say-reason-clusters speaker reasons)))
                    (cluster-objs
                     (mapcar #'car
                          (group-reasons (decision-downside-record decision)))))))
             (paragraph
              (cons (concatenate 'string intro " " (car reason-list))
                    (cdr reason-list))
              stream))))

    (values)
    )

(defun gen-record-np (memb)  
  (let ((pronoun (gen-pronoun memb 'poss)))
    (proto-dnp *record*
               'lex-list
               (list "the record"
                     (concatenate 'string pronoun " record")
                     (concatenate 'string pronoun " voting record")))))

               
               

(defun gen-downside-intro (decision)  
  (let ((memb (decision-member decision))
        (verb (gen-pick-one
               "realize"
               "understand"
               "recognize"
               "know"
               "appreciate"
               "see")))
    (format nil "~A, ~A ~A that"
            (gen-pick-one
             "However"
             "Still"
             "But"
             "Even so"
             "At the same time")
            (gen-np memb 'subj)
            (gen-verb memb verb nil 'present))))
             



(defun gen-downside-record-intro (decision)  
  (let ((memb (decision-member decision))
        (verb (gen-pick-one
               "note"
               "believe"
               "assert"
               "affirm"
               "declare"
               "maintain")))
    (format nil "~A, ~A ~A that"
            (gen-pick-one
             "However"
             "Still"
             "But"
             "Even so"
             "At the same time")
            (gen-np memb 'subj)
            (gen-verb memb verb nil 'present))))
             

;;------------------------------------------------------------------------
;;  pro/con statements -- issue subjects
;;------------------------------------------------------------------------

(defun english-record-pro (subject stance)  
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "~A firm support for" (gen-verb subject "demonstrate" nil 'present))
          (format nil "~A strong support for" (gen-verb subject "show" nil 'present))))

    ((b) (gen-pick-one
          (format nil "~A support for" (gen-verb subject "demonstrate" nil 'present))
          (format nil "~A support for" (gen-verb subject "show" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A support for" (gen-verb subject "reveal" nil 'present))))))

(defun english-record-con (subject stance)  
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "~A firm opposition to" (gen-verb subject "demonstrate" nil 'present))
          (format nil "~A strong opposition to" (gen-verb subject "show" nil 'present))))

    ((b) (gen-pick-one
          (format nil "~A opposition to" (gen-verb subject "demonstrate" nil 'present))
          (format nil "~A opposition to" (gen-verb subject "show" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A opposition to" (gen-verb subject "reveal" nil 'present))))))





