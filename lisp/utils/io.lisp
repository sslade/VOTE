

;;  routines for interactive data entry


;;----------------------------------------------------------
;;      Prompts and Data Entry 
;;----------------------------------------------------------

(defun print_prompt (prompt)  
    (format t "~&~A" prompt)
    (finish-output nil))  ;; 11/5/2015 

(defun prompted-string-input (prompt)  
    (print_prompt prompt)
    (lread))

(defun prompted-list-input (prompt)  
    (read-from-string
	 (concatenate 'string "(" (prompted-string-input prompt) ")")))

;; remove commas from string input before converting to symbols
(defun read-string->list (input)
    (read-from-string
	 (concatenate 'string "(" (substitute #\space #\, input) ")")))

(defun prompted-symbol-input (prompt)  
  (let ((input (prompted-string-input prompt)))
    (if input
      (->symbol input)
      nil)))


(defun ->symbol (obj)
  (cond ((symbolp obj) obj)
        ((stringp obj) (read-from-string obj))
        ((characterp obj) (->symbol (string obj)))
        (else obj)))

(defun prompted-yes-no-input (prompt)  
  (case (prompted-symbol-input prompt)
    ((yes y t true) T)
    ((no n nil false f) nil)
    (otherwise
     (format t "~%Type Yes or No to prompt.~%")
     (prompted-yes-no-input prompt))))

(defun prompted-s-exp-input (prompt)  
  (print_prompt prompt)
  (read))

(defun prompted-date-input (prompt)  
  (let ((input (prompted-list-input prompt)))
    (cond ((null input) '())
          (else (list->date input)))))

(defun prompted-time-input (prompt)  
  (let ((input (prompted-list-input prompt)))
    (cond ((null input) '())
          (else (list->time input)))))

;;  (string-list->string '("this" "is" "a" "test"))
;; " this is a test "
;;-------------------------------------------------
(defun string-list->string (lst)  
  (cond ((null lst) " ")
	(else 
         (concatenate 'string " " (car lst) (string-list->string (cdr lst))))))

;; bug? does not seem to take empty line, unless preceded by text.
;; e.g., (lread) followed by CR is no op.
(defun lread ()
  (clear-input *standard-input*)
  (let ((input (read-line *standard-input*)))
    (cond ((string= "" input) nil)
          (else input))))

(defun prompted-string-list-input (prompt)  
  (labels ((aux-read
		  (result)
		  (let ((input (prompted-string-input "> ")))
		    (cond (input (aux-read (cons input result)))
				(else (reverse result))))))
    (format t "~%Enter text for ~A (<CR><CR> to get back to command level):~%" 
              prompt)
    (aux-read nil)))

(defun ->string (x)  
  (format nil "~A" x))

;;----------------------------------------------------------
;;      Output routines
;;----------------------------------------------------------

(defun print-list (stream lst)  
    (mapc 
        #'(lambda (item)
            (format stream "~A " item))
        lst))

(defun pp-tab-list (stream lst)  
  (let ((widest (apply #'max
                       (mapcar 
                        #'(lambda (n) (length 
                                     (->string n)))
                        lst))))
    (tab-pp stream lst (+ widest 2))))

(defun tab-pp (stream lst tab-stop)  
  (cond ((NULL lst)
         (terpri stream))
        (else
         (format stream "~%~A" (car lst))
         (tab-print stream (cdr lst) tab-stop))))

(defun tab-print (stream lst tab-stop)  
  (cond ((null lst)
         (terpri stream))
        (else
         (tab stream tab-stop)
         (if (>= (hpos stream) (- (line-length stream)
                                (length 
                                 (->string (car lst)))))
             (terpri stream))
         (format stream "~A" (car lst))
         (tab-print stream (cdr lst) tab-stop))))

(defun tabular-print (stream lst tab-stop)  
  (cond ((null lst)
         (format stream "\\\\ \\hline ~%"))
        (else
         (tab stream tab-stop)
         (if (>= (hpos stream) (- (line-length stream)
                                (length 
                                 (->string (car lst)))))
             (terpri stream))
;; don't print zero data
         (format stream "&~a" (if (and (numberp (car lst))
                                     (zerop (car lst)))
                                " "
                                (car lst)))
         (tabular-print stream (cdr lst) tab-stop))))


(defun tab (stream stop-width)  
  (let ((args (list (format nil "~~1,~DT" stop-width))))
    (apply #'format stream args))) 


(defparameter *debug-msg* nil)

;; DEBUG-MSG prints out a message to the terminal if the *DEBUG-MSG* flag is set.

(defun debug-msg (&rest items)  
    (if *debug-msg* (apply #'format (append `(,*standard-output*) items)))
    (values ))

;; NOTDEBUG-MSG prints out a message to the terminal if the *DEBUG-MSG* flag is NOT set.

(defun notdebug-msg (&rest items)  
    (if (not *debug-msg*) (apply #'format (append `(,*standard-output*) items)))
    (values))


;; T's display procedure
(defun display (obj stream)
  (write obj :stream stream)
  (values))

;; handle vagaries of different merge-pathnames
(defun append-pathname (path ext)
  (pathname (format nil "~A.~A" (namestring path) ext)))
