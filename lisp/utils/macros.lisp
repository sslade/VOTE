
;;; The *my-read-table*, to hold the ! comment and @ unhash read-macros.
;-----------------------------------------------------------------
(DEFVAR *my-read-table*  (copy-readtable nil))


;;; Chris' ! comment macro

;;; the ! comment readmacro --  ignores everything till a line with ( in the
;;; first column is seen.  If ! is not followed by whitespace, an atom starting
;;; with ! is generated and returned.
;-----------------------------------------------------------------
(defun comment-macro (stream char)  
  (declare (ignore char))
  (labels ((gobble_comment (stream)
            (let ((c (read-char stream nil eof)))
             (cond ((eof? c) c)
                   ((and (char= c #\newline)
                         (char= (peek-char nil stream nil eof) #\())
                    (values))
                  (else (gobble_comment stream))))))
   (let ((c (peek-char nil stream nil eof)))
    (cond ((eof? c) c)
          ((whitespace? c) (gobble_comment stream))
          (t (INTERN (format nil "!~S" (read stream))))))))

(set-macro-character #\! #'comment-macro nil *my-read-table*)

;;  @4  returns the object whose hash value is 4
;;  (inspired by D. Kranz)
;-----------------------------------------------------------------
(defun unhash-macro (stream char)  
  (declare (ignore char))
  (list 'object-unhash (read stream t nil t)))

(set-macro-character #\@ #'unhash-macro nil *my-read-table*)

;;  change the standard read-table
;-----------------------------------------------------------------
(defun my-readtable ()
  (setq *readtable*  *my-read-table*))

;;  lambda abbreviation
;-----------------------------------------------------------------
(defmacro ^ (&rest rest)  
    `#'(lambda . ,rest))

;;  macros from T book chapter 11
;-----------------------------------------------------------------
;; (repeat 5 (format t "Hello~%"))
(defmacro repeat (n &rest body)
  `(flet ((code () ,@body))
    (do ((count ,n (- count 1)))
        ((<= count 0) nil)
        (code))))

;; (while (not (zerop q)) (format t "~d~%" (decf q)))
(defmacro while (test &rest body)  
 `(flet ((test () ,test)
	    (body () ,@body))
    (do ()
        ((null (test)) nil)
        (body))))

;; (until (zerop q) (format t "~d~%" (decf q)))
(defmacro until (test &rest body)  
 `(flet ((test () ,test)
	    (body () ,@body))
    (do ()
        ((test) nil)
        (body))))


(defmacro pp (obj)
  `(pretty-print ,obj *standard-output*))

(defmacro define-predicate (type)
  `(defun ,(read-from-string (format nil "~A?" type)) (obj)
     (typep obj ',type)))


(defmacro defpredicate (type)
 `(defun ,(read-from-string (format nil "~A-P" type)) (obj)
     (typep obj ',type)))


;  `(defgeneric ,(read-from-string (format nil "~A-P" class)) (obj)
;     (:documentation ,(format nil "A predicate for the ~A class" class))
;     (:method ((obj ,class)) T)
;     (:method (obj) nil)))


;; create executable data structures, e.g., hash tables and arrays
;; see chapter 14 of LISP book
(defmacro make-executable (item)
  `(typecase ,item
     (hash-table
      (setf (symbol-function ',item)
            #'(lambda (key) (gethash key ,item)))
      (defun (setf ,item) (val key) 
        (setf (gethash key ,item) val))
      ',item)
     (array
      (setf (symbol-function ',item)
            #'(lambda (&rest indices) (apply #'aref ,item indices)))
      (defun (setf ,item) (val &rest indices)
        (setf (apply #'aref ,item indices) val))
      ',item)
     (T 'not-an-appropriate-type)))


;; e.g., (function-synonym plus +)

(defmacro function-synonym (f1 f2)
  `(setf (symbol-function ',f1) (symbol-function ',f2)))
