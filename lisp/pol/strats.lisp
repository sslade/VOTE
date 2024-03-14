
;; As of 9/25/90
;;  

;;....STRATEGY> h
;;
;;      0   Popular decision                        [A]  @@@(POPULAR)
;;      1   Inconsistent constituency               [B]  @@@(INCONSISTENT-CONSTITUENCY)
;;      2   Non-partisan decision                   [B]  @@@(NON-PARTISAN)
;;      3   Not constitutional                      [B]  @@@(NOT-CONSTITUTIONAL)
;;      4   Unimportant Bill                        [B]  @@@(UNIMPORTANT-BILL)
;;      5   Balance the books                       [C]  @@@(BALANCE-THE-BOOKS)
;;      6   Best for the country                    [C]  @@@(BEST-FOR-THE-COUNTRY)
;;      7   Minimize adverse effects                [C]  @@@(MINIMIZE-ADVERSE-EFFECTS)
;;      8   Not good enough                         [C]  @@@(NOT-GOOD-ENOUGH)
;;      9   Partisan Decision                       [C]  @@@(PARTISAN)
;;      10  Shifting alliances                      [C]  @@@(SHIFTING-ALLIANCES)
;;      11  Simple consensus                        [C]  @@@(SIMPLE-CONSENSUS)
;;      12  Normative decision                      [D]  @@@(NORMATIVE)
;;      13  Simple Majority                         [D]  @@@(SIMPLE-MAJORITY MAJORITY)
;;  *   14  Deeper analysis                         [E]  @  (DEEPER-ANALYSIS)
;;      15  No decision                             [F]  @ @(NO-DECISION)
;;      16  Change of heart                         [X]  @@@(CHANGE-OF-HEART)
;;      17  Innoculation                            [X]  @@@(INNOCULATION)
;;      18  It couldn't pass                        [X]  @@@(IT-COULD-NOT-PASS)
;;      19  Mixed constituency                      [X]   @@(MIXED-CONSTITUENCY)
;;      20  Unpopular decision                      [X]   @@(UNPOPULAR)
;;

;;------------------------------------------------------------------
;;  firm-decision  sets final outcome of decision structure
;;------------------------------------------------------------------

(defun firm-decision (decision side reasons old-downside strat)  
  (let* ((downside
          (filter 
           (flatten old-downside)
           #'(lambda (st) (eq (id (reveal-source st))
                             (id (decision-bill decision))))))
         (record (collect-bills downside)))
    (setf (decision-result decision) side)
    (setf (decision-reason decision) reasons)
    (setf (decision-strategy decision) strat)
    (cond (record
           (setf (decision-downside-record decision) record)
           (setf (decision-downside decision)
                (remove-intersection downside record #'eq)))
          (else
           (setf (decision-downside decision) downside)))
  decision))


(defun set-decision-outcome (decision result strat)  
  (let ((reason (case result
                  ((for) (decision-for-stances decision))
                  ((agn) (decision-agn-stances decision))))
        (downside (case result
                    ((agn) (decision-for-stances decision))
                    ((for) (decision-agn-stances decision)))))
    (firm-decision decision result reason downside strat)))

;;==================================================================
;;      0   Popular decision                        [A] @(POPULAR)
;;
;;  Remarks:       Vote is consistent with major constituencies.
;;  Quote:         I just try to vote my district.
;;                 I was sent to Washington to represent the way people back home feel.
;;                 This is what the vast majority want.
;;                 I owe it to my constiuents if they feel that strongly about it. [Delegate stance]
;;  Rank:          "A"
;;  Test:          All stances on one side of bill.
;;  Test-code:     STRAT-POPULAR
;;  Example:       (VOTE 'BRUCE 'PLANT-CLOSING)
;;==================================================================

(defun strat-popular (decision strat)  
  (let ((for-stances (decision-for-stances decision))
        (agn-stances (decision-agn-stances decision)))

    (cond ((and (null for-stances)
                agn-stances)
           (firm-decision decision 'agn agn-stances nil strat))
          ((and (null agn-stances)
                for-stances)
           (firm-decision decision 'for for-stances nil strat))
          (else nil))))


;;==================================================================
;;      1   Inconsistent constituency               [B] @(INCONSISTENT-CONSTITUENCY)
;;    Same group on both sides of issue
;;==================================================================

(defun strat-inconsistent-constituency (decision strat)  
  (let* ((source-conflicts (decision-split-group decision))
         (result (consensus decision)))
    (cond ((null source-conflicts)
           nil)
          (result
           (set-decision-outcome decision result strat))
          (else
           nil))))

(defun majority (decision)  
  (let ((fors (decision-number-for decision))
        (agns (decision-number-agn decision)))
    (cond ((not (and (numberp fors)
                     (numberp agns)))
           nil)
          ((> fors agns) 'for)
          ((> agns fors) 'agn)
          (else nil))))
        
(defun consensus (decision)  
  (let ((MI (mapcar #'car (collect-MI decision))))
    (if (= (length (remove-duplicates MI)) 1)
        (car MI)
        nil)))

(defun collect-MI (decision)  
  (delete '()
        (list
         (decision-MI-stance decision)
         (decision-MI-group decision)
         (decision-MI-credo decision)
         (decision-MI-record decision)
         (decision-MI-norm decision))))


;;==================================================================
;;      2   Non-partisan decision                   [B]  (NON-PARTISAN)
;;  
;;  Remarks:       Vote of conscience or credo that violates party line.  Not a district vote.
;;  Quote:         Sometimes party loyalty demands too much. (JFK)
;;  Rank:          "B"
;;  Test:          Major conflict between credo and party stances.
;;==================================================================

;;  The vote is a matter of conscience.
;;  The credo position is in conflict with a party position.
;;  The credo position is very important.
;;  

(defun strat-non-partisan (decision strat)  
  (let* ((credo (decision-MI-credo decision))
         (credo-side (car credo))
         (credo-stance-list (cadr credo))
         (opposing-groups (case credo-side
                            ((for) (decision-group-agn decision))
                            ((agn) (decision-group-for decision))))
         (party (case (member-party (decision-member decision))
                  ((rep) 'republicans)
                  ((dem) 'democrats))))
    (cond ((and credo
                opposing-groups
                credo-stance-list
                (member party (mapcar #'stance-source opposing-groups))
                (most-important? (stance-importance (car credo-stance-list))))
           (set-decision-outcome decision credo-side strat))
          (else nil))))
           
             

;;==================================================================
;;      3   Not constitutional                      [B]  (NOT-CONSTITUTIONAL)
;;
;;  Remarks:       Vote against a measure that would be struck down by
;;                 the Supreme Court.
;;  Rank:          "B"
;;==================================================================

             
(defun strat-not-constitutional (decision strat)  
  (let ((result (consensus decision)))
    (cond ((and (EQ  result 'agn)
                (collect
                 (decision-agn-stances decision)
                 #'(lambda (stance) (eq (reveal-issue stance)
                                       (get-node 'constitution issue)))))
           (let* ((reason (decision-agn-stances decision))
                  (downside (decision-for-stances decision)))
             (firm-decision decision result reason downside strat)))
        (else nil))))



;;==================================================================
;;      4   Unimportant Bill                        [B]   (UNIMPORTANT-BILL)
;;  
;;  Date-open:     Monday, May 22, 1989
;;  Symbol:        STRATEGY.681
;;  Name:          "Unimportant Bill"
;;  Sort-key:      "BUnimportant Bill"
;;  Synonyms:      (UNIMPORTANT-BILL)
;;  Isa-depth:     ""
;;  Remarks:       Not much riding on this bill.
;;  
;;  Quote:         [Morrison:] some things that are close calls are not treated
;;                 as close calls because they're not important enough.  I mean
;;                 its very different if there's enough riding -- either substantively
;;                 or politically -- on a vote.  You might have exactly the same
;;                 tensions among the various priorities if you were to pull 
;;                 this up, but it might be about how you spend $100,000 and you
;;                 say, fuck this.
;;  
;;  Rank:          "B"
;;  Test:          Importance of bill is minimal.
;;==================================================================


(defun strat-unimportant-bill (decision strat)  
  (let ((result (consensus decision))
        (import (bill-importance (decision-bill decision))))
    (cond ((and result
                (eq import 'C))
           (set-decision-outcome decision result strat))
          (else nil))))


;;==================================================================
;;      5   Balance the books                       [C]  (BALANCE-THE-BOOKS)
;;
;;  Remarks:       Offset current vote with past or future votes.
;;  Quote:         I know you are upset with this vote, but I have always been there in the
;;                   past, and I shall be there in the future.
;;                 I will make it up to you.
;;                 (point to specific past votes)
;;  Rank:          "C"
;;==================================================================


(defun strat-balance-the-books (decision strat)  
  (let ((result (majority decision))
        (split  (decision-split-record decision)))
    (cond ((and result split)
           (set-decision-outcome decision result strat))
          (else nil))))


;;==================================================================
;;      6   Best for the country                    [C]  (BEST-FOR-THE-COUNTRY)
;;
;;  Remarks:       Take the broad view, over parochial interests.
;;  Quote:         The needs of the country, in this case, must come first.
;;  Rank:          "C"
;;  Test:          National interest in conflict with local interest.
;;==================================================================

;; relies on a group: country which has the broad issue agenda for
;;  the whole country.  Each congressman has a positive relation with COUNTRY.


(defun strat-best-for-country (decision strat)  
  (let* ((result (consensus decision))
         (country (get-node 'country group))
         (country-for (collect (decision-group-for decision)
                              #'(lambda (st) (eq country (reveal-source st)))))
         (country-agn (collect (decision-group-agn decision)
                              #'(lambda (st) (eq country (reveal-source st))))))
    (cond ((and (eq result 'for)
                country-for
                (null country-agn))
           (set-decision-outcome decision result strat))
          ((and (eq result 'agn)
                country-agn
                (null country-for))
           (set-decision-outcome decision result strat))
          (else nil))))



;;==================================================================
;;      7   Change of heart                         [C]  (CHANGE-OF-HEART)
;;  
;;  Remarks:       Reverse a credo/vote position on the record to accomodate
;;                 conflict in constituencies.
;;  Quote:         A foolish consistency is the hobgoblin of small minds.
;;  Rank:          "C"
;;  Test:          Credo importance is less than conflicting relation importance.
;;==================================================================

(defun strat-change-of-heart (decision strat)  
  (let ((result (majority decision))
        (split  (decision-split-credo decision)))
    (cond ((and result split)
           (set-decision-outcome decision result strat))
          (else nil))))



;;==================================================================
;;      8   Inoculation                            [C]  (INOCULATION)
;;  
;;  Remarks:       Decision which may prove to be unpopular later on.
;;                 Need to begin laying groundwork for defense early on.
;;  Rank:          "C"
;;  Test:          Low priority stances, pro or con.
;;==================================================================

(defun strat-inoculation (decision strat)  
  (let* ((result (majority decision))
         (split-groups (and (decision-group-for decision)
                            (decision-group-agn decision)))
         (import-level (if split-groups
                         (stance-importance
                          (car (msort (append (decision-group-for decision)
                                              (decision-group-agn decision))))))))
    (cond ((and result split-groups (<important? import-level 'B))
           (set-decision-outcome decision result strat))
          (else nil))))




;;==================================================================
;;      9   It couldn't pass                        [C]  (IT-COULD-NOT-PASS)
;;
;;  Remarks:       Do not waste a vote on a symbolic measure.  Better to
;;                 build credibility and a consensus for the future.
;;  Quote:         Why waste a vote on a measure that has so little chance of passing.
;;  Rank:          "C"
;;  Test:          Bill has far higher importance (and low likelihood of passage)
;;                 but for issue stance consistent (but stronger than) with my own.
;;                 This is the flip side of not-good-enough.
;;==================================================================


(defun strat-could-not-pass (decision strat)  
  (let ((result (majority decision))
        (billid (decision-bill decision))
        )
    (cond ((and (eq result 'agn)
                (could-not-pass? billid))
           (set-decision-outcome decision result strat))
          (else nil))))


(defun might-pass? (billid)  
  (let ((tally (bill-vote-tally billid)))
    (and tally
         (or (approved? billid)
             (> (vote-ratio billid) 1)))))

(defun could-not-pass? (billid)  
  (let ((tally (bill-vote-tally billid)))
    (and tally
         (< (vote-ratio billid) 1))))


(defun vote-ratio (billid)  
  (let* ((tally (collect (bill-vote-tally billid) #'numberp))
         (fors (car tally))
         (agns (cadr tally)))
    (and fors agns
         (/ fors agns))))
    

(defun approved? (bill)  
  (member (car (bill-vote-tally bill))
         '(PASSED ADOPTED APPROVED)))

(defun rejected? (bill)  
  (member (car (bill-vote-tally bill))
         '(REJECTED FAILED)))


;;==================================================================
;;      10  Minimize adverse effects                [C]  (MINIMIZE-ADVERSE-EFFECTS)
;;
;;  Remarks:       Adverse effects are less important than the benefits of the vote.
;;  Quote:         Nothing's perfect.  You have to break a few eggs to make
;;                 omelets.
;;  Rank:          "C"
;;  Test:          The downside results are lower in importance than the upside.
;;==================================================================


(defun strat-minimize-adverse-effects (decision strat)  
  (let* ((result (majority decision))
         (MI-up-level (and result (get-MI-level decision result)))
         (MI-down-level (and result (get-MI-level decision (opposite-result result)))))
    (cond ((and result (>important? MI-up-level MI-down-level))
           (set-decision-outcome decision result strat))
          (else nil))))


(defun get-MI-level (decision result)  
  (let ((stances (case result
                   ((for) (decision-for-stances decision))
                   ((agn) (decision-agn-stances decision))
                   (else nil))))
    (if stances
        (stance-importance (car (msort stances)))
        'D)))



;;==================================================================
;;      11  Mixed constituency                      [C]  (MIXED-CONSTITUENCY)
;;
;;  Remarks:       E.g., rural/urban. can justify pro-rural vote to urbans by
;;                 pointing to other constituency, and vice-versa.
;;  Rank:          "C"
;;  Test:          District must be divided and there should be symmetry on stances.
;;==================================================================


;;;  note - same as balance the books ...


;;==================================================================
;;      12  Not good enough                         [C]  (NOT-GOOD-ENOUGH)
;;
;;  Remarks:       Bill importance is less than my own stances would call for.
;;                 For example, personal stance of A, bill importance of C.
;;  Quote:         I would have voted for a stronger bill.
;;                 This measure is a fraud.  It has no teeth in it.
;;  Rank:          "C"
;;  Test:          Compare importance of bill stance with own stances.  Check for disparity.
;;==================================================================



(defun strat-not-good-enough (decision strat)  
  (let* ((result (majority decision))
         (MI-up-level (and result (get-MI-level decision result)))
         (MI-bill-level (and result (get-MI-bill-level decision result))))
    (cond ((and (eq result 'for)
                (>important? MI-up-level MI-bill-level))
           (set-decision-outcome decision 'AGN strat))
          (else nil))))



(defun get-MI-bill-level (decision result)  
  (let ((stances (case result
                   ((for) (bill-stance-for (decision-bill decision)))
                   ((agn) (bill-stance-agn (decision-bill decision)))
                   (else nil))))
    (if stances
        (stance-importance (car (msort stances)))
        nil)))



;;==================================================================
;;      13  Partisan Decision                       [C]  (PARTISAN)
;;
;;  Remarks:       Counter-planning vote against the opposing interests.
;;  Quote:         I could not let those rich Republicans tear this country apart.
;;                 I am not going to let those Democrats have their special interests tell
;;                   me what to do.
;;  Rank:          "C"
;;  Test:          Voting to deny something wanted by opposition.
;;==================================================================


(defun strat-partisan (decision strat)  
  (let* ((result (majority decision))
         (MI-up-level (and result (get-MI-level decision result)))
         (MI-con-rel-level (and result
                                (update-con-rel-stances decision)
                                (get-MI-con-rel-level decision (opposite-result result)))))
    (cond ((and result
                MI-con-rel-level
                (<important? MI-up-level MI-con-rel-level))
           (set-decision-outcome decision result strat))
          (mi-con-rel-level
           (my-format t "~%Partisan Decision...")
           nil)
          (else
           nil))))

(defun update-con-rel-stances (decision)  
  (setf (decision-con-rel-for-stances decision) 
       (match-con-rel-stances-for/agn decision 'for))

  (setf (decision-con-rel-agn-stances decision) 
       (match-con-rel-stances-for/agn decision 'agn))
  'done)


(defun get-MI-con-rel-level (decision result)  
  (let ((stances (case result
                   ((for) (decision-con-rel-for-stances decision))
                   ((agn) (decision-con-rel-agn-stances decision))
                   (otherwise nil))))
    (if stances
        (stance-importance (car (msort stances)))
        nil)))


;;; match-con-rel-stances will check
;;; con group/relationship stances
;;;

(defun match-con-rel-stances (stance-id mem-id)  
  (my-format t "~5T~A~&"
             stance-id)
  (let ((matches (mapcar 
                  #'(lambda (mem-stance)
                    (match? stance-id mem-stance))
                  (member-con-rel-stances mem-id))))

    (delete '() matches)))


(defun match-con-rel-stances-for/agn (decision side)  
  (out-switch
   (progn
     (indent-box so (format nil "Vote ~A ~A"
			    side (bill-bnumber (decision-bill decision))))
     (format t "~%Considering counter-planning implications of vote ~A ~A~&"
	     side (bill-bnumber (decision-bill decision)))
     (my-format t "Matching member con-rel stances with bill stances:~&")
     )
   )
  (let* ((bill-stance (if (eq side 'for)
                          #'bill-stance-for
                          #'bill-stance-agn))
         (sort-key (or (member-stance-sort-key (decision-member decision))
                       'equity))
         (stances (mappend
                   #'(lambda (stance)
                     (match-con-rel-stances stance (decision-member decision)))
                   (apply bill-stance (list (decision-bill decision))))))

    (my-format t "Sorting stances based on ~A order..." sort-key)
    (mapc 
     #'(lambda (stance)
       (set-sort-key stance sort-key))
     stances)
    (setf stances (msort stances))
    (my-format t "done.~%")
    (my-format t "Stances ~A: " side)
    (out-switch (my-pretty-print stances so))
    stances))




;;==================================================================
;;      14  Shifting alliances                      [C]  (SHIFTING-ALLIANCES)
;;
;;  Remarks:       Conflict resolution through changing relations.
;;                 When two constituencies are in conflict, try to see if one is more
;;                 valuable or compatible than the other.  If so, shift their relative
;;                 importance by either lowering one or raising the other.
;;  Quote:         I am voting with my good friends in ().  I am sorry to disappoint
;;                 my many supporters in ().  They know that I have fought many fights
;;                 with them.
;;  Rank:          "C"
;;  Test:          Two constituencies in conflict.
;;                 Identify other important issue in which they disagree which is
;;                 important to me.  Pick the one that sides with me.
;;==================================================================

(defun strat-shifting-alliances (decision strat)  
  (let ((result (divided-groups decision)))
    (cond (result
           (set-decision-outcome decision result strat))
          (else nil))))


;;==================================================================
;;        15  Simple consensus                        [C] @ (SIMPLE-CONSENSUS)
;;  
;;  Status:        "Active"
;;  Date-open:     Thursday, May 11, 1989
;;  Symbol:        STRATEGY.1018
;;  Name:          "Simple consensus"
;;  Sort-key:      "CSimple consensus"
;;  Synonyms:      (SIMPLE-CONSENSUS)
;;  Isa-depth:     ""
;;  Remarks:       The most important issues/groups/norms etc. concur.
;;  
;;  Rank:          "C"
;;  Test:          Check all most important features
;;  
;;  Test-code:     STRAT-SIMPLE-CONSENSUS
;;==================================================================


(defun strat-simple-consensus (decision strat)
  (let ((result (consensus decision)))
    (cond (result
           (set-decision-outcome decision result strat))
          (else nil))))


;;==================================================================
;;    *   16  Deeper analysis                         [D+]   (DEEPER-ANALYSIS)
;;  
;;  Status:        "Active"
;;  Date-open:     Sunday, February 11, 1990
;;  Symbol:        STRATEGY.323
;;  Name:          "Deeper analysis"
;;  Synonyms:      (DEEPER-ANALYSIS)
;;  Isa-depth:     ""
;;  Remarks:       Consider the symbolic implication of the for/agn stances of the bill.
;;  
;;  Rank:          "D+"
;;  Test:          Find a consensus after expanding the bill-stance through inference.
;;==================================================================


(defun strat-deeper-analysis (decision strat)  
  (let ((level (decision-deeper-analysis decision)))
    (cond ((null level)
           (progn 
             (new-analysis-level decision)
           ;;  The neg stance matching was for testing.
           ;;  (set-decision-neg-stances decision)
             (reanalyze-decision decision)
             (apply-strats decision
                           (filter (db-all strategy) #'strategy-no-second-try)))) 
          (else (strat-deeper-analysis2 decision strat)))))

(defun strat-deeper-analysis2 (decision strat)  
  (let* ((level (new-analysis-level decision))
         (billid (decision-bill decision))
         (old-bill-for-stances (bill-stance-for billid))
         (old-bill-agn-stances (bill-stance-agn billid))
         (strats (filter (db-all strategy) #'strategy-no-second-try))
         (new-bill-for-stances (expand-stances old-bill-for-stances level))
         (new-bill-agn-stances (expand-stances old-bill-agn-stances level))
         )

    ;; hack to make an implicit temporary variable using block0

    (setq new-bill-for-stances
         (prog1 
          (remove-intersection new-bill-for-stances
                               new-bill-agn-stances
                               #'stance-equal?)
          (setq new-bill-agn-stances
               (remove-intersection new-bill-agn-stances
                                    new-bill-for-stances
                                    #'stance-equal?))))
    (cond ((eq level 'D) nil)
          ((or (> (length new-bill-for-stances)
                  (length old-bill-for-stances))
               (> (length new-bill-agn-stances)
                  (length old-bill-agn-stances)))
           (my-format t "~%Deeper Analysis results in new bill stances at level ~A...~%" level)

           (print-new-stances new-bill-for-stances old-bill-for-stances 'for)
           (print-new-stances new-bill-agn-stances old-bill-agn-stances 'agn) 

           (setf (bill-stance-for billid)  new-bill-for-stances)
           (setf (bill-stance-agn billid)  new-bill-agn-stances)

           (reanalyze-decision decision)

           (prog1 
            (apply-strats decision strats)
            (setf (bill-stance-for billid) old-bill-for-stances)
            (setf (bill-stance-agn billid) old-bill-agn-stances)))
          (else
           (my-format t "~%No change at this level.  Trying deeper Deeper Analysis.~%")
           (strat-deeper-analysis decision strat)))))


(defun reanalyze-decision (decision)  
  (out-switch
   (center-box so "Re-Analyzing alternative positions"))

  (setf (decision-for-stances decision) 
       (match-stances-for/agn decision 'for))

  (setf (decision-agn-stances decision) 
       (match-stances-for/agn decision 'agn))

  (update-decision-metrics decision))



(defun new-analysis-level (decision)  
  (let* ((old-level (decision-deeper-analysis decision))
         (new-level (next-analysis-level old-level)))
    (setf (decision-deeper-analysis decision) new-level)
    new-level))

(defun next-analysis-level (level)  
  (case level
    ((X) 'A)
    ((A) 'B)
    ((B) 'C)
    ((C) 'D)
    (otherwise 'X)))


;;  (define (apply-decision-strategies decision)
;;    (out-switch (center-box so "Applying decision strategies"))
;;    (let ((strats (db-all strategy)))
;;      (apply-strats decision strats)))
;;  
;;  expand-stances infers as much as possible from a list of stances
;;  with importance greater than or equal to given level
;;------------------------------------------------------------------

(defun expand-stances (stance-list level)  
  (remove-duplicates
   (append stance-list
           (mappend #'(lambda (st) (expand-one-stance st level))
                    stance-list))))


;;  (define (expand-stances stance-list level)
;;    (remove-duplicates
;;     (expand-stance-list stance-list level nil)))
;;  
;;  
(defun expand-stance-list (stances level result)
  (if (null stances) 
    result
    (let ((st-list (expand-one-stance (car stances) level)))
      (cond (st-list
             (expand-stance-list (union (cdr stances) (set-filter st-list result))
                                 level
                                 (cons (car stances) result)))
            (else
             (expand-stance-list (cdr stances) level (cons (car stances) result)))))))

;;  Expand the stances that are of importance greater than or equal to the
;;  given level of importance.
;;  
(defun expand-one-stance (stance level)  
  (let ((side (stance-side stance))
        (iss  (reveal-issue stance)))
    (collect
     (case side
       ((pro) (issue-pro-stances iss))
       ((con) (issue-con-stances iss))
       (otherwise nil))
     #'(LAMBDA (st)
         (>=important? (stance-importance st) level)))))


;;  (set-filter '(1 2 3 4) '(2 4 6))
;;------------------------------------
(defun set-filter (set1 set2)  
  (filter set1
          #'(lambda (item) (member item set2))))


(defun strat-simple-majority (decision strat)  
  (let ((result (majority decision)))
    (cond (result
           (set-decision-outcome decision result strat))
          (else nil))))



(defun print-new-stances (new old side)  
  (cond ((> (length new) (length old))
         (my-format t "~%New ~A stances resulting from deeper analysis:~%" side)
         (mapcar 
          #'(lambda (stance)
            (my-format t "~2T~A~%" stance))
          (remove-intersection new old #'stance-equal?)))
        (else nil)))




;;==================================================================
;;      17  Normative decision                      [D] @(NORMATIVE)
;;
;;  Remarks:       Decision reflects normative opinion on relevant issues.
;;  Rank:          "D"
;;  Test:          Bill stances match normative stances for given issues.
;;  Test-code:     STRAT-NORMATIVE
;;==================================================================

(defun strat-normative (decision strat)  
  (let* ((for-norms (decision-for-bnorms decision))
         (agn-norms (decision-agn-bnorms decision)))
    
    (cond ((and (null for-norms)
                agn-norms)
           (firm-decision decision 'agn agn-norms nil strat))
          ((and (null agn-norms)
                for-norms)
           (firm-decision decision 'for for-norms nil strat))
          (else nil))))


;;==================================================================
;;      18  Unpopular decision                      [D]  (UNPOPULAR)
;;
;;  Remarks:       Recognizes that vote will not play well with many constituents.
;;  Quote:         I know people will criticize me for this vote, but I had to do it.
;;                 I couldn't live with myself if I didn't.
;;                 I have to use my judgment.  You can express an opinion.  I have to make a decision. [Trustee]
;;                 I stand by my record.  I'm not going to change my principles.
;;                 I hope you appreciate that and will look at my record as a whole.
;;  Rank:          "D"
;;  Test:          Major conflicts among stances.
;;==================================================================



;;==================================================================
;;      19  No decision                             [E] @(NO-DECISION)
;;  Test-code:   STRAT-NO-DECISION
;;  Remarks:     No previous decision was triggered.
;;  Test:        Always true.
;;==================================================================
(defun strat-no-decision (decision strat)  
  (firm-decision decision nil nil nil strat))
