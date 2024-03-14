;; Simple strategy: always vote along party lines
;; Republicans will support president and democrats will
;; oppose the president.
;; *** need to change for change in White House ***
(defun strat-party (decision strat)  
  (let* ((pres-pos (bill-pres-pos (decision-bill decision)))
         (party (member-party (decision-member decision)))
         (choice (case party
                   ((rep) pres-pos)
                   ((dem) (case pres-pos
                            ((for) 'agn)
                            ((agn) 'for))))))
    (set-decision-outcome decision choice strat)))

