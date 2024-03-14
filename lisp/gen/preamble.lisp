

;;;;------------------------------------------------------------------------
;;;;    generic preamble code
;;;;------------------------------------------------------------------------


(defun preamble (decision port)  
  (paragraph (say-result decision) port)
  (let* ((strat (decision-strategy decision))
         (phrase (cond ((strategy-preamble strat)
                        (funcall (strategy-preamble strat) decision))
                       (else nil))))
    (cond (phrase
           (paragraph phrase port))
          (else nil))))


(defun say-result (decision)  
  (let* ((phrase (pick-vote-phrase (decision-result decision)))
         (verb   (if (stringp phrase)
                     phrase
                     (car phrase)))
         (memb   (decision-member decision))
         (billid (decision-bill decision)))
    (format nil "~A ~A~A bill ~A, ~A."
            (gen-np memb 'subj)
            (gen-verb memb verb nil 'present)
            (if (stringp phrase)
                ""
                (concatenate 'string " " (cadr phrase)))
            (bill-bnumber billid)
            (english billid))))

;;; ***** add bill-english, instead of bill-name

(defun pick-vote-phrase (result)  
  (case result
    ((for) (gen-pick-one
            "support"
            "favor"
            '("be" "in favor of")
            '("be" "backing")
            `("be" "for")
            '("vote" "for")
            "endorse"
            "back" ))
    ((agn) (gen-pick-one
            '("be" "opposed to")
            '("be" "against")
            '("vote" "against")
            "oppose"))
    (otherwise (gen-pick-one
             '("be" "undecided about")
             '("be" "indifferent about")
             "abstain"))))




;;;;------------------------------------------------------------------------
;;;;     preamble code for specific strategies
;;;;------------------------------------------------------------------------
          
(defun preamble-non-partisan (decision)  
  (let* ((subj (decision-member decision)))
    (format nil "~A ~A that ~A should be a matter of conscience, not partisan politics."
            (gen-np subj 'subj)
            (gen-verb subj "believe" nil 'present)
            (eng-stance-issue (caadr (decision-MI-credo decision))))))
 


(defun preamble-believe-template (decision string-for string-agn)  
  (let* ((subj (decision-member decision))
         (deep (decision-deeper-analysis decision))
         (string (if string-agn
                     (case (decision-result decision)
                       ((for) string-for)
                       ((agn) string-agn))
                     string-for))
         (outstring (if deep
                        (concatenate 'string 
                         (gen-pick-one
                          "after careful analysis, "
                          "after weighing the implications, "
                          "after due consideration, "
                          "upon reflection, ")
                         string)
                        string)))
    (format nil outstring
            (gen-np subj 'subj)
            (gen-verb subj "believe" nil 'present))))
         
(defun preamble-not-constitutional (decision)  
  (preamble-believe-template
   decision
   "~A ~A that provisions of this bill are not constitutional."
   nil))

  

(defun preamble-unimportant-bill (decision)  
  (preamble-believe-template
   decision
   "~A ~A that though provisions of this bill may be controversial, they are not of major concern."
   nil
   ))


(defun preamble-popular (decision)  
  (preamble-believe-template
   decision
   "~A ~A this bill to be in the best interests of the people."
   "~A ~A this bill not to be in the best interests of the people."))


(defun preamble-inconsistent-constituency (decision)  
  (preamble-believe-template
   decision
   "~A ~A that sincere people have trouble balancing the tradeoffs presented by this bill."
   nil))


(defun preamble-balance-the-books (decision)  
  (preamble-believe-template
   decision
   "~A ~A that the people who disagree with this vote may find reassurance in the record."
   nil))


(defun preamble-best-for-the-country (decision)  
  (preamble-believe-template
   decision
   "~A ~A this bill to be in the best interests of the country as a whole."
   "~A ~A this bill not to be in the best interests of the country as a whole."))


(defun preamble-change-of-heart (decision)  
  (preamble-believe-template
   decision
   "~A ~A this vote represents a change of heart."
   nil))


(defun preamble-inoculation (decision)  
  (preamble-believe-template
   decision
   "~A ~A this bill deals with issues that are going to continue to be important to the people."
   nil))


(defun preamble-it-could-not-pass (decision)  
  (preamble-believe-template
   decision
   "~A ~A this bill had very little chance of passing in any event."
   nil))


(defun preamble-minimize-adverse-effects (decision)  
  (preamble-believe-template
   decision
   "~A ~A the adverse effects of this bill are far outweighed by other issues."
   "~A ~A the beneficial effects of this bill far outweigh other issues."))


(defun preamble-mixed-constituency (decision)  
  (preamble-believe-template
   decision
   "~A ~A this bill may make some people unhappy, but will be of great benefit to the majority."
   "~A ~A this bill may make some people happy, but will be of great harm to the majority."))


(defun preamble-not-good-enough (decision)  
  (preamble-believe-template
   decision
   "~A ~A that this bill does not go far enough."
   nil))


(defun preamble-partisan (decision)  
  (let* ((subj (decision-member decision))
         (party (member-party subj))
         (opening (format nil "~A ~A not going to let "
                          (gen-np subj 'subj)
                          (gen-verb subj "be" nil 'present))))
    (concatenate 'string 
     opening
     (case party
       ((rep) "those Democrats have their special interests tell us what to do.")
       ((dem) "those rich Republicans tear this country apart.")
       (else nil)))))


(defun preamble-shifting-alliances (decision)  
  (preamble-believe-template
   decision
   "~A ~A that sincere people have honest differences of opinion on this bill."
   nil))


(defun preamble-simple-consensus (decision)  
  (preamble-believe-template
   decision
   "~A ~A that the consensus of opinion supports this measure."
   "~A ~A that the consensus of opinion is opposed to this bill."))

(defun preamble-unpopular (decision)  
  (preamble-believe-template
   decision
   "~A ~A that popular opinion supports this measure."
   "~A ~A that popular opinion is opposed to this bill."))


(defun preamble-normative (decision)  
  (preamble-believe-template
   decision
   "~A ~A that this bill accurately reflects the concerns of most people."
   "~A ~A that this bill fails to reflect the way most people feel."))


(defun preamble-simple-majority (decision)  
  (preamble-believe-template
   decision
   "~A ~A that the majority of people support this measure."
   "~A ~A that the majority of people oppose this bill."))




