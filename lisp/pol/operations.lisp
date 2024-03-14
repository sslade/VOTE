

(defgeneric synonyms (self))
(defgeneric english-short (obj)
  (:method (obj) nil))

(defgeneric set-isa-sort (self))
(defgeneric set-alpha-sort (self))
(defgeneric set-date-sort (self))

;; from issue

(defgeneric get-stances (self side))

;; from stance
(defgeneric set-sort-key (self keyword))

(defgeneric isa (self)
  (:method (self) (slot-value self 'isa)))


(defgeneric reveal-group (self))

(defgeneric reveal-issue (self)
  (:method (self) nil))

(defgeneric match? (alpha beta)
  (:method (alpha beta) (equalp alpha beta)))

;; from stance
(defgeneric reveal-source (self))


;;  Data dependencies:
;;  
;;  Decision may change if any of the following change:
;;  
;;      issue-norm
;;      group-stances
;;      bill-importance
;;      bill-stance-for
;;      bill-stance-agn
;;      member-credo
;;      member-votes (member-stances)
;;      member-relations (member-pro-rel-stances member-con-rel-stances)
;;  
;;      
;;      Additions to the database will not directly affect prior decisions.
;;      Only when changes are made to items that were used in the original
;;      decision, e.g., issues, groups, bills, and members.
;;      
;; returns list of selectors that may affect a decision.

(defgeneric decision-dependencies (self)
  (:method (self) nil))

