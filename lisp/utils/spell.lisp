;; remove words from given string or list that are in the stop-table
;;  or are numbers
;; (filter-keyword '(a b c d) stop-table)
;; -----------------------------------------------------------------
(defun filter-keywords (name stop-table)  
  (let ((word-list (if (stringp name)
                     (setq name (read-string->list name))
                     name)))
    (filter word-list #'(lambda (word)
                          (or (numberp word)
                              (gethash word stop-table))))))


;; stop-table for items commonly not to be indexed
;; -----------------------------------------------------------------
(defvar stop-table (make-hash-table))
;; initialize table of stop words -- not to be indexed
(mapc #'(lambda (word) (setf (gethash word stop-table) T))
      '(a b c d e f g h i j k l m n o p q r s t u v w x y z
        jr sr dr phd mr mrs ms col maj
        a. b. c. d. e. f. g. h. i. j. k. l. m. n. o. p. q.
        r. s. t. u. v. w. x. y. z. 
        jr. sr. dr. ph.d. mr. mrs. ms. col. maj.))

;;(make-executable stop-table)
(setf (symbol-function 'stop-table)
      #'(lambda (key) (gethash key stop-table)))
(defun (setf stop-table) (val key) 
  (setf (gethash key stop-table) val))


;; ------------------------------------------------------
;; spelling correction
;; from Chapter 6, section 5 of T book
;;  added table argument.  this must an executable table, 
;;  a la make-my-table
;; (spell-match hash-table-name word)
;; ------------------------------------------------------
(defun spell-match (table word)  
  (cond ((not (stringp word)) (setf word (->string word))))
  (if (or (null word) 
          (string= "" word))
    nil
    (let ((word-length (length word)))
      (cond ((< word-length 2) nil)
            ((sm-check table word))
            ((sm-deletion table word word-length))
            ((sm-transposition table word (- word-length 1)))
            ((sm-double table word (- word-length 1)))
            ((sm-insertion table word word-length))
            (else nil)))))

(defun sm-check (table word)  
  (cond ((stringp word) (setq word (intern (string-upcase word)))))
  (cond ((gethash word table) word)
        (else nil)))

(defun sm-deletion (table word-string index)  
  (cond ((zerop index) nil)
        ((let ((new-word
                (concatenate 'string 
                             (subseq word-string 0 (- index 1))
                             (subseq word-string index))))
           (sm-check table new-word)))
        (else (sm-deletion table word-string (- index 1)))))

(defun sm-transposition (table word-string index)  
  (cond ((zerop index) nil)
        ((sm-check table (string-swap word-string index (- index 1))))
        (else (sm-transposition table word-string (- index 1)))))

(defun sm-double (table word-string index)  
  (cond ((minusp index) nil)
        ((sm-check table (string-insert word-string 
                                        (elt word-string index)
                                        index)))
        (else (sm-double table word-string (- index 1)))))

(defun sm-insertion (table word-string index)  
  (cond ((minusp index) nil)
        ((sm-insertion-check table word-string index #\A))
        (else (sm-insertion table word-string (- index 1)))))

(defun sm-insertion-check (table word-string index new-char)  
  (cond ((char> new-char #\Z) nil)
        ((sm-check table (string-insert word-string new-char index) ))
        (else (sm-insertion-check table word-string index 
                                  (incr-char new-char)))))

(defun incr-char (character)  
  (code-char (+ 1 (char-code character))))

(defun string-swap (string index1 index2)  
  (cond ((or (not (stringp string))
             (minusp index1)
             (minusp index2)
             (>= index1 (length string))
             (>= index2 (length string))
             )
         'Error-in-string-swap)
        (else
         (let ((new-string (copy string))
               (temp-char (elt string index1)))
           (setf (elt new-string index1) (elt new-string index2))
           (setf (elt new-string index2) temp-char)
           new-string))))


(defun string-insert (string new-char index)  
  (cond ((or (not (stringp string))
             (not (characterp new-char))
             (not (integerp index))
             (> index (length string))
             (minusp index))
         'error-in-string-insert)
        (else
         (concatenate 'string 
                      (subseq string 0 index)
                      (string new-char)
                      (subseq string index)))))
