

;---------------------------------------------------
;;  debug format routine
;---------------------------------------------------

;;  (db-format t "In routine foo with args: ~A~%" args)
;;  (setq db-format T)     --- turns on debug printing
(defvar db-format nil)
(declaim (special db-format))

(defun db-format (stream &rest args)  
  (if db-format (progn
                  (format stream "~%** Debug: ")
                  (apply #'format stream args)))
  (values ))


;---------------------------------------------------
;;  Structure utilities
;---------------------------------------------------
; 
; (defun initialize-entire-structure (stype value)  
;     (MAPC 
;         #'(LAMBDA (selector)
;           (SETF (selector (stype-master stype)) value))
;         (stype-selectors stype)))
; 
; (defun reveal-struct (struct)  
;   (let ((stype (structure-type struct)))
;     (MAPCAR #'(LAMBDA (selector)
;        (let ((component (selector struct)))
;          (cond ((structure? component)
;                 (reveal-struct component))
;                (else component))))
;         (stype-selectors stype))))
; 
; 
;;   Assume: (define-structure-type score home visitor)
;;
;;   (default score 'home 0 'visitor 0)
;;   ==>
;;   (*DEF SCORE-STYPE 'HOME 0 'VISITOR 0)
; 
; (DEFMACRO default (name . l)  
;   (let* ((stype (concatenate-symbol name '-stype)))
;     `(*def ,stype ,@l)))
; 
; ;; *def works 
; ;;   (*def score-stype 'home 0 'visitor 0)
; (defun *def (stype . l)  
;   (let ((master (stype-master stype)))
;     (iterate next ((l l))
;         (cond ((NULL l) nil)
;               (else
;                  (let ((selector (stype-selector stype (car l)))
;                        (value (cadr l)))
;                    (SETF (selector master) value)
;                    (next (cddr l))))))))
; 
;;--------------------------------------------------
;;  Macro utilities   
;;--------------------------------------------------
;; OK:  1/12/93
(defun ma (exp)  
  (macroexpand exp))

(defun ppma (exp)  
  (pprint (ma exp) *STANDARD-OUTPUT*)
  (VALUES))

;;--------------------------------------------------
;;  Abbreviations
;;--------------------------------------------------

;;(DEFVAR bt backtrace)
;;(DEFVAR args argspectrum)
;;(DEFVAR db debug)

(defun ou (o) (object-unhash o))
(defvar so *standard-output*)
(defvar si *standard-input*)

;;--------------------------------------------------
;;  Environment utilities
;;--------------------------------------------------
;; 
;; (defun clean-slate ()
;;   (let ((env (repl-env)))
;;     (define *clean-env* (make-locale standard-env '*clean-env*))
;;     (*define *clean-env* 'dirty-slate 
;;         #'(LAMBDA () (SETF (repl-env) env)))
;;     (SETF (repl-env) *clean-env*)))
;; 
;; 
;; (defun pause (. msg)  
;;     (fresh-line *STANDARD-OUTPUT*)
;;     (APPLY #'format *STANDARD-OUTPUT* msg)
;;     (format *STANDARD-OUTPUT* "~%** Pausing (continue with P): ")
;;     (iterate again ()
;;       (let ((input (read *STANDARD-INPUT*)))
;;         (cond ((EQ  input 'p) nil)
;;               (else 
;;                (print (eval input (repl-env))
;;                       *standard-output*)
;;                (format t "~%Pause> ")
;;                (again))))))
;;         
;;--------------------------------------------------
;;  Sorting routines
;;--------------------------------------------------
    
;;  > (msort '(5 4 3 6 2 4 7 5))
;;  (2 3 4 4 5 5 6 7)
;;  > (msort '("this" "that" "these" "those"))
;;  ("that" "these" "this" "those")
;;--------------------------------------------------
(defun msort (l)  
  (labels ((sort2 (l tmplist)
             (cond ((null l) (sort3 tmplist nil))
                   (else (sort2 (cdr l)
                                (sort-add (list (car l)) tmplist)))))
           (sort3 (l tmplist)
             (cond ((null l) tmplist)
                   (else (sort3 (cdr l) (smerge (car l) tmplist)))))
           (sort-add (x tmplist)
             (cond ((null tmplist) (list x))
                   ((null (car tmplist)) (cons x (cdr tmplist)))
                   (else
                    (cons nil (sort-add (smerge x (car tmplist))
                                        (cdr tmplist)))))))
    (sort2 l nil)))

;;--------------------------------------------------
(defun smerge (a b)  
  (cond ((null a) b)
        ((null b) a)
        ((inorder? a b)
         (cons (car a)
               (smerge (cdr a) b)))
        (else
         (cons (car b)
               (smerge (cdr b) a)))))

;; (pretty-print object *standard-output*)
;;------------------------------------------------
(defgeneric inorder? (first second)
  (:documentation "Should the first come before the second?")
  (:method ((first cons) (second cons)) (inorder? (car first) (car second)))
  (:method ((first string) (second string)) (string< first second))
  (:method ((first number) (second number)) (< first second))
  (:method ((first symbol) (second symbol)) (string-lessp first second))
  (:method ((first character) (second character)) (char< first second))
  )


;;  from T book, exer. 6.6.2
;;--------------------------------------------------
;; (defun string-less? (first second)  )  --> STRING<

;;  from T book, page 105 and 378
(defun capitalize (object)  
  (if (symbolp object) (setq object (string object)))
  (string-capitalize object))

(defun capitalize-one (object)  
  (if (symbolp object) (setf object (string object)))
  (string-capitalize object :start 0 :end 1))

(defun string-append (&rest strings)
  (cond ((car strings)
         (format nil "~A~A" (car strings)
                 (apply #'string-append (cdr strings))))
        (else "")))

;;; remove items from given list that satisfy predicate
;; ----------------------------------------------------
(defun filter (lst pred)  
  (labels ((filter-tr (lst pred result)
             (cond ((null lst)
                    (nreverse result))
                   (else
                    (filter-tr (cdr lst)
                               pred
                               (if (funcall pred (car lst))
                                 result
                                 (cons (car lst) result)))))))
    (filter-tr lst pred nil)))


;;; collect items from given list that satisfy predicate
;;  ----------------------------------------------------
(defun collect (lst pred)  
  (labels ((collect-tr (lst pred result)
             (cond ((null lst)
                    (reverse result))
                   (else
                    (collect-tr (cdr lst)
                                pred
                                (if (funcall pred (car lst))
                                  (cons (car lst) result)
                                  result))))))
    (collect-tr lst pred nil)))

;; combination of map and append.  Returns a flat list of results.
;;  > (mappend cdr '((a b c) (d e f) (g h i)))
;;  (B C E F H I)

(defun mappend (proc lst)  
  (cond ((null lst) nil)
        (else
         (append (funcall proc (car lst))
                 (mappend proc (cdr lst))))))


(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (cons tree nil))
        (else
         (append (flatten (car tree)) (flatten (cdr tree))))))

