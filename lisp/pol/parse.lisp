
;;  
;;  This routine will analyze a bill-remarks phrase and produce the likely
;;  issue outcomes pro/con for the bill.
;;


;; word-list is a sequence of words.  returns list of issues that match.
;;----------------------------------------------------------------------
(defun match-with-issues (word-list)  
  (if (null word-list) 
    nil
    (let ((match (match-one-word-with-issues (car word-list))))
      (cond (match
             (append match
                     (match-with-issues (cdr word-list))))
            (else
             (match-with-issues (cdr word-list)))))))


(defvar bill-stop-table)
(setf bill-stop-table (make-hash-table)) 

;; initialize table of stop words -- not to be indexed
(mapc #'(lambda (x) (setf (gethash x bill-stop-table) T))
      '(a b c d e f g h i j k l m n o p q r s t u v w x y z
        jr sr dr phd mr mrs ms col maj
        a. b. c. d. e. f. g. h. i. j. k. l. m. n. o. p. q. r. s. t. u. v. w. x. y. z. 
        jr. sr. dr. ph.d. mr. mrs. ms. col. maj.
        the of and to on no for with committee government american
        if one an at support proposal program
        in as ban case use act department amendment us)) ;; federal


(defun match-one-word-with-issues (word)  
  (let* ((table (db-table ISSUE))
         (match (spell-match table word)))
    (cond ((gethash word bill-stop-table) nil)
          ((numberp word) nil)
          ((table word))
          (match
           (my-format t "~%Matching with spelling correction: ~A => ~A~%"
                      word match)
           (table match))
          (else nil))))


(defun most-frequent (word-list)  
  (let ((next-list (keep-duplicates word-list)))
    (cond ((null word-list) nil)
          ((null next-list) word-list)
          (else
           (most-frequent next-list)))))


(defun list-of-strings->list (lst)  
  (flatten (mappend
            #'(lambda (str)
              (read-string->list (process-punctuation str)))
            lst)))

(defun extract-bill-issues (word-list)  
  (most-frequent
   (cond ((double-match-with-issues word-list))
         (else
          (my-format t "~%Double match failed.  Trying single match.~%")
          (match-with-issues word-list)))))


(defun infer-bill-stances (billid &rest args)  
  (let* ((word-list (list-of-strings->list (remarks billid))) 
         (issues (extract-bill-issues word-list))
         (opp (opposing-issue? word-list))
         (stance-for (mapcar 
                      #'(lambda (iss) (get-norms-or-new-stances iss opp))
                      issues))
         (stance-agn (mapcar #'for->agn-stance stance-for)))
    (setf (bill-i-stance-for billid) stance-for)
    (setf (bill-i-stance-agn billid) stance-agn)
    (case (car args)
      ((for) stance-for)
      ((agn) stance-agn)
      (otherwise
       (list
        'FOR= stance-for
        'AGN= stance-agn)))))

(defun get-norms-or-new-stances (issueid oppose-flag)  
  (let* ((norm (issue-norm issueid))
         (stance (cond (norm (copy norm))
                       (else
                        (init-stance `(,(car (synonyms issueid)) pro b bill bill))))))
    (if oppose-flag
        (for->agn-stance stance)
        stance)))

(defun opposing-issue? (word-list)  
  (cond ((null word-list) nil)
        ((numberp (car word-list))
         (opposing-issue? (cdr word-list)))
        ((gethash (car word-list) opposition-table))
        (else
         (opposing-issue? (cdr word-list)))))

(defvar opposition-table)
(setf opposition-table (make-hash-table))

;; initialize table of opposition keywords
(mapc #'(lambda (x) (setf (gethash x opposition-table) T))
      '(prohibiting prohibition striking limit opposition oppose bar against))
              

(defun for->agn-stance (stance)  
  (let ((new-stance (copy stance))
        (opp (issue-opposite (reveal-issue stance))))
    (cond (opp
           (setf (stance-issue new-stance) (id opp))
           (reveal-issue new-stance))
          (else
           (setf (stance-side new-stance)
                (if (eq 'pro (stance-side new-stance))
                    'con
                    'pro))))
    new-stance))


(defun double-match-with-issues (word-list)  
  (cond ((null word-list) nil)
        (else
         (double-match-words (match-one-word-with-issues (car word-list))
                             (cdr word-list)))))

(defun double-match-words (word word-list)  
  (cond ((null word-list) nil)
        (else
         (let* ((next-word (match-one-word-with-issues (car word-list)))
                (match (intersection word next-word)))
           (cond (match
                  (append match
                          (double-match-words next-word (cdr word-list))))
                 (else
                  (double-match-words next-word (cdr word-list))))))))


(defun process-punctuation (str)  
  (cond ((string= "" str) "")
        (else
         (concatenate 'string 
          (case (char str 0)
            ((#\. ) " *period* ")
            ((#\, ) " *comma* ")
            ((#\? ) " *question* ")
            ((#\$ ) " *dollar* ")
            ((#\( ) " *lparen* ")
            ((#\) ) " *rparen* ")
            ((#\' #\` #\") " *quote* ")
            (otherwise  (->string (char str 0))))
          (process-punctuation (subseq str 1))))))
