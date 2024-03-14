
;;;;------------------------------------------------------------------------
;;;;    relations
;;;;------------------------------------------------------------------------

;;  State a generic relation


(defun eng-relation (subject relation)  
  (concatenate 'string (gen-np subject 'subj)
                 (eng-relation2 subject relation)
                 (eng-relation-group relation)
                 "."))

(defun eng-relation2 (subject relation)  
  (let* ((side (relation-side relation)))
    (format nil " ~A "
            (case side
              ((pro) (english-pro subject relation))
              ((con) (english-con subject relation))))))


;; 9/9/16
; (function-synonym english-rel-pro english-pro) 
; (function-synonym english-rel-con english-con) 

(defun eng-relation-group (relation)  
  (let* ((grp (reveal-group relation))
         (side (relation-side relation))
         (imp? (<important? 'C (relation-importance relation)))
         (phrase (english-group-phrase grp side))
         (said  (said? *context* grp))
         (short (english-short grp))
         (grp-eng (if imp?
                      (case side
                        ((pro) (if (sarcasm?)
                                   (group-con-english grp)
                                   (group-pro-english grp)))
                        ((con) (if (sarcasm?)
                                   (group-pro-english grp)
                                   (group-con-english grp))))
                      nil)))
    (format nil "~A~A"
            (if (and (not said) phrase)
                (concatenate 'string phrase " ")
                "")
            (cond (grp-eng)
                  ((and said short))
                  (said
                   (english grp))
                  (else (english grp)))
            )))


;; the intro phrase is used the first time an group is mentioned
;; in the current context.

(defun english-group-phrase (iss side)  
  (declare (ignore iss) (ignore side))
  nil)
;;;;    
;;;;      (case (group-type iss)    
;;;;        ((principle) "the principle of")
;;;;        ((right)     "the right")
;;;;        ((proposal)  "the proposal")
;;;;        ((policy)    "the policy of")
;;;;        ((group)     (case side
;;;;                       ((pro) "the legitimate rights of")
;;;;                       ((con) "the special interests of")))
;;;;        ((cause)     "the cause of")
;;;;        ((program)   "the program of")
;;;;        ((nomination) "the nomination and appointment of")
;;;;        ((reform)    (gen-pick-one
;;;;                      "efforts to achieve a better"
;;;;                      "the reform of"))
;;;;        ((problem)   "the problem of")
;;;;        ((foreign)   (gen-pick-one
;;;;                      "American involvement in"
;;;;                      "our country's role in"
;;;;                      "our country's relations with"))
;;;;        (else        nil)))             ;; was "the group of"
;;;;        
;;;;



;;  
(defun eng-test-group (side imp grp-list)  
  (let ((groups (mapcar 
                 #'(lambda (grp)
                   (if (group-p grp)
                       grp
                       (get-node grp group)))
                 grp-list)))
    (mapcar 
     #'(lambda (grp)
       (cond ((group-norm grp)
              (format nil "~A"
                      (eng-relation (gen-norm-np) (group-norm grp))))
             (else
              (format nil "~A (*)"
                      (eng-relation *self*
                                    (init-relation
                                     (list grp side imp (get-node 'aclu group) 'group)))
                      ))))
     groups)))



;;;;------------------------------------------------------------------------
;;;;    cluster relations
;;;;------------------------------------------------------------------------

(defun say-relations (subj relations)  
  (paragraph
   (mapcar 
    #'(lambda (relation)
      (format nil "~A"
              (eng-relation subj relation)))
    relations)
   so))

(defun say-cluster-relations (subj stream)  
  (paragraph
   (mapcar #'(lambda (cluster) (say-group-clusters subj cluster))
        (cluster-objs (member-relations subj)))
   stream))


(defun say-group-clusters (subject relation-list)  
  (if (null (cdr relation-list))
      (eng-relation subject (car relation-list))
      (concatenate 'string (gen-np subject 'subj)
                     (eng-relation2 subject (car relation-list))
                     (eng-relation-clusters relation-list)
                     ".")))


(defun eng-relation-clusters (relation-list)  
  (cond ((null (cdr relation-list))
         (concatenate 'string (gen-conjunction)
                        (eng-relation-group (car relation-list))))
        (else
         (concatenate 'string (eng-relation-group (car relation-list))
                        ", "
                        (eng-relation-clusters (cdr relation-list))))))
