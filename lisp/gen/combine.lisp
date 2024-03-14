

;; want to combine stances with different groups
;; used in explaining groups that share downside objections.

(defun combine-stances (stance-list)  
  (mapcar 
   #'(lambda (st)
     (setf (stance-siblings st)
          (collect 
           stance-list
           #'(lambda (st2)
             (and (not (eq st st2))
                  (stance-alikev? st st2)))))
     st)
   stance-list))


;** 9/9/16
(defvar *siblings*)
(declaim (special *siblings*))

;(dnpq *siblings*
;      person third
;      number plur
;      human t
;      gender unspec
;      )

(dnp '*siblings*
      'person 'third
      'number 'plur
      'human 't
      'gender 'unspec
      )

(defun create-sibling-np (stance)  
  (let* ((sibs (stance-siblings stance))
         (phrase (generate-sibling-phrase sibs)))
    (proto-dnp *siblings*
               'lex phrase)))

(defun generate-sibling-phrase (sibs)  
  (cond ((null sibs) "")
        (else
         (concatenate 'string 
          (english (reveal-source (car sibs)))
          (cond ((and (cdr sibs)
                      (NULL (cddr sibs)))
                 (gen-pick-one
                  ", and "
                  ", and "
                  ", and "
                  ", as well as "
                  ", in addition to "))
                ((cdr sibs)
                 ", ")
                (else ""))
          (generate-sibling-phrase (cdr sibs))))))
                  
