

(defun protocol-popular (decision)  
  (let ((result (decision-result decision))
        (reasons (decision-reason decision)))
    (my-format t "~%All stances are ~A this bill:" result)
    (my-format t "~%    ")
    (out-switch
     (my-pretty-print reasons *standard-output*))
    (my-format t "~%There are no reasons to vote ~A this bill.~%"
               (opposite-result result))))

          
(defun protocol-non-partisan (decision)  
  (let* ((credo (decision-MI-credo decision))
         (credo-side (car credo))
         (credo-stance-list (cadr credo))
         (opposing-groups (case credo-side
                            ((for) (decision-group-agn decision))
                            ((agn) (decision-group-for decision))))
         (party (case (member-party (decision-member decision))
                  ((rep) 'republicans)
                  ((dem) 'democrats)))
         (party-stance (collect
                         opposing-groups
                         #'(lambda (st) (eq party (stance-source st))))))
    (my-format t "~%The member's party (~A) has a stance ~A this bill:~%~10T"
               party (opposite-result credo-side))
    (my-pretty-print party-stance *standard-output*)
    (my-format t "~%While the member has a strong personal stance ~A the bill:~%~10T"
               credo-side)
    (my-pretty-print credo-stance-list *standard-output*)
    ))           


(defun protocol-not-constitutional (decision)  
  (protocol-simple-consensus decision)
  (my-format t "~%There are constitutional grounds for opposing this bill:~%~15T")
  (my-pretty-print
   (collect
    (decision-agn-stances decision)
    #'(lambda (stance) (eq (reveal-issue stance)
                          (get-node 'constitution issue))))
   *standard-output*))


(defun protocol-unimportant-bill (decision)  
  (protocol-simple-consensus decision)
  (my-format t "~%And this bill has a low level of importance: ~A~%"
             (bill-importance (decision-bill decision))))


(defun protocol-inconsistent-constituency (decision)  
  (protocol-simple-consensus decision)
  (let ((groups (decision-split-group decision)))
    (my-format t "~%The same group~P has stances on both sides of this bill: ~%~15T~A"
             (length groups) groups)
    ))


(defun protocol-balance-the-books (decision)  
  (protocol-simple-majority decision)
  (my-format t "~%The record supports positions on both sides of the bill:~%~15TFOR: ")
  (my-pretty-print (collect-bills (decision-for-stances decision))
                   *standard-output*)
  (my-format t "~%~15TAGN: ")
  (my-pretty-print (collect-bills (decision-agn-stances decision))
                   *standard-output*)
  )  


(defun protocol-best-for-the-country (decision)  
  (protocol-simple-consensus decision)
  (let* ((result (decision-result decision))
         (country (get-node 'country group))
         (country-stance (collect
                          (case result
                            ((for) (decision-group-for decision))
                            ((agn) (decision-group-agn decision))
                            (otherwise nil))
                          #'(lambda (st) (eq country (reveal-source st))))))
    (my-format t "~%The country as a whole has a stance ~A this bill: ~%~15T"
               result)
    (my-pretty-print country-stance *standard-output*)))


(defun protocol-minimize-adverse-effects (decision)  
  (protocol-simple-majority decision)
  (let ((result (decision-result decision))
        (mi-for (car (msort (decision-for-stances decision))))
        (mi-agn (car (msort (decision-agn-stances decision)))))
    (my-format t
               "~%The high priority ~A stance is more important than the high priority ~A stance."
               result (opposite-result result))
    (my-format t "~%~15T~A: " result)
    (my-pretty-print (case result
                       ((for) mi-for)
                       ((agn) mi-agn))
                     *standard-output*)
    (my-format t "~%~15t~a: " (opposite-result result))
    (my-pretty-print (case (opposite-result result)
                       ((for) mi-for)
                       ((agn) mi-agn))
                     *standard-output*)
    ))


(defun protocol-not-good-enough (decision)  
  (protocol-simple-majority decision)
  (my-format t "~%Even though the majority opinion favors this bill, the bill is too weak.
The importance of the agenda stances is greater than the bill stances.
Therefore, decide to vote against the bill in protest.")
  (let ((mi-up-stance (car (msort (decision-for-stances decision))))
        (bill-up-stance (car (msort (bill-stance-for (decision-bill decision))))))
    (my-format t "~%~15TStrong agenda stance: ")
    (my-pretty-print mi-up-stance *standard-output*)
    
    (my-format t "~%~15TWeak bill stance: ")
    (my-pretty-print bill-up-stance *standard-output*)
    ))


(defun protocol-partisan (decision)  
  (protocol-simple-majority decision)
  (let* ((result (decision-result decision))
         (pro-stances (case result
                        ((for) (decision-for-stances decision))
                        ((agn) (decision-agn-stances decision))))
         (con-rel-stances (case result
                            ((for) (decision-con-rel-agn-stances decision))
                            ((agn) (decision-con-rel-for-stances decision)))))
    (my-format t "~%Voting ~A this bill also thwarts the opposition, for
whom this bill is of greater importance:~%~15TOur side: " result)
    (my-pretty-print pro-stances *standard-output*)
    (my-format t "~%~15TTheir side: ")
    (my-pretty-print con-rel-stances *standard-output*)))


(defun protocol-shifting-alliances (decision)  
  (let ((result (decision-result decision))
        (fors (msort (remove-intersection (decision-group-for decision)
                                          (decision-group-agn decision)
                                          #'stance-relation-alikev?)))
        (agns (msort (remove-intersection (decision-group-agn decision)
                                          (decision-group-for decision)
                                          #'stance-relation-alikev?))))
  (my-format t "~%There is no credo stance involved in this vote.
There are groups on either side of this bill:~%~15TFOR: ")
  (my-pretty-print fors *standard-output*)
  (my-format t "~%~15TAGN: ")
  (my-pretty-print agns *standard-output*)
  (my-format t "~%The member has belief conflicts with the ~A group (noted above),
so the decision is with the ~A group.~%"
             (opposite-result result) result)
  ))



(defun protocol-simple-consensus (decision)  
  (let ((result (decision-result decision)))
    (my-format t "~%Found a consensus ~A this bill." result)
    (my-format t "~%The most important stances are all on the ~A of this bill:~%" result)
    (mapc 
     #'(lambda (pair)
       (let ((slot (car pair))
             (string (cadr pair)))
         (cond ((slot-value decision slot)
                (my-format t "  ~A: ~20T" string)
                (out-switch
		 (progn  ;; 11/24/15
		   (my-pretty-print (slot-value decision slot) *standard-output*)
		   (my-format t "~%"))))
               (else nil))))
     '((mi-group "Group")
       (mi-credo "Credo")
       (mi-record "Record")
       (mi-norm "Norm")))
    ))




(defun protocol-normative (decision)  
  (let ((for-norms (decision-for-bnorms decision))
        (agn-norms (decision-agn-bnorms decision))
        (result (decision-result decision)))
    (my-format t "~%Public opinion norms are all ~A this bill:~%~15T"
               result)
    (my-pretty-print (case result
                       ((for) for-norms)
                       ((agn) agn-norms))
                     *standard-output*)
    (my-format t "~%There are no norms ~A this bill.~%"
               (opposite-result result))))


(defun protocol-simple-majority (decision)  
  (let ((result (majority decision)))
    (my-format t "~%Found a simple majority ~A this bill." result)
    (print-majority-stances decision result)
    (print-majority-stances decision (opposite-result result))))


(defun print-majority-stances (decision result)  
  (let* ((stances (case result
                    ((for) (decision-for-stances decision))
                    ((agn) (decision-agn-stances decision))))
         (count (length stances)))
    (my-format t "~%There ~A ~A ~A stance~P:~%~28T"
               (if (> count 1)
                   "are"
                   "is only")
               count result count)
    (out-switch
     (my-pretty-print stances *standard-output*)))
  )

     

(defun protocol-no-decision (decision)  
    (my-format t "~%VOTE has failed to arrive at a decision.~%"))
    
