
;; from chapter 7 of T book
(defconstant eof '**EOF**)

(defun eof? (x) (eq x eof))

(defun write-spaces (stream count)
  (let ((args (list (format nil "~~~D@A" count) "")))
    (apply #'format stream args)))

(defun indent (text indentation stream)  
    (TERPRI stream)
    (write-spaces stream indentation)
    (write-string (->string text) stream)
    (VALUES))

(defun flushleft (text stream)  
    (indent text 0 stream))

(defun flushright (text stream)  
    (let ((indentation (- (line-length stream)
                          (length text))))
       (indent text indentation stream)))

(defun center (text stream)  
    (let ((indentation (truncate (- (line-length stream)
                                    (length text))
                            2)))
       (indent text indentation stream)))

(defun split1 (left-text right-text stream)  
    (flushleft left-text stream)
    (write-spaces stream
                  (- (line-length stream)
                     (+ (hpos stream)
                        (length right-text))))
    (write-string right-text stream)
    (values))


(defun stream-copy (s-input s-output)  
    (cond ((or (not (input-stream-p s-input))                        
               (not (output-stream-p s-output)))
           (write-string
                "Foul stream in STREAM-COPY."
			  *error-output*))
          ((eof? (peek-char nil s-input nil eof))   
           '*END-OF-STREAM-COPY*)
          (else                         
           (write-string (read-line s-input) s-output)
           (terpri s-output)
           (stream-copy s-input s-output))))

(defun type-file (file-name)  
    (let ((file-stream (open file-name :direction :input)))
       (stream-copy file-stream *STANDARD-OUTPUT*)
       (close file-stream)))

(defun filter-file (filter input-file output-file)  
    (let ((in-stream (open input-file :direction :input))
          (out-stream (open output-file :direction :output)))
      (filter-stream filter in-stream out-stream)
      (close in-stream)
      (close out-stream)))

(defun filter-stream (filter in-stream out-stream)  
    (cond ((eof? (peek-char nil in-stream nil eof)) 'EOF)
          (else
           (let ((tmp (apply filter (list (read in-stream)))))
            (cond (tmp
                   (format out-stream "~D~%" tmp)))
            (filter-stream filter in-stream out-stream)))))

;; > (filter-file #'3-letter-word? "f1" "f2")
;;  (defun 3-letter-word? (word) 
;;      (cond ((symbolp word) (setq word (->string word))))
;;      (cond ((not (stringp word)) NIL)
;;            ((= (length word) 3) word)
;;            (else nil)))

;; Exercises and answers

(defun column-print (list indentation stream)  
    (cond ((NULL list)
           (TERPRI stream)
           (VALUES))
          (else
           (indent (car list) indentation stream)
           (column-print (cdr list) indentation stream))))

(defun split (left-text right-text stream)  
    (flushleft left-text stream)
    (write-spaces stream
                  (- (line-length stream)
                     (+ (LENGTH left-text)
                        (LENGTH right-text))))
    (write-string right-text stream)
    (VALUES))


;; replaces write-spaces with repeat-char

(defun tab-to-column (column stream)  
    (cond ((>= (hpos stream) column)
           (TERPRI stream)))
    (repeat-char stream #\space (- column (hpos stream)))
    (VALUES))

(defun split-fill (left-text char right-text stream)  
    (flushleft left-text stream)
    (repeat-char stream char
                 (- (line-length stream)
                    (+ (hpos stream)
                       (LENGTH right-text))))
    (write-string right-text stream)
    (VALUES))

(defun repeat-char (stream char count)  
    (cond ((ZEROP count) (VALUES))
          (else (write-char char stream)
                (repeat-char stream char (- count 1)))))

;; *********************

(defun revise-file (input-file output-file old new)  
    (let ((in-stream  (open input-file :direction :input))
          (out-stream (open output-file :direction :input)))
        (revise-stream in-stream out-stream old new)
        (close in-stream)
        (close out-stream)
        '*end-of-file-revision*))

(defun revise-stream (in out old new)  
    (cond ((eof? (peek-char nil in nil eof))
           '*end-of-stream-revision*)
          (else
           (let ((line (string-subst (read-line in) old new T)))
              (write-string line out)
              (TERPRI out)
              (revise-stream in out old new)))))

(defun string-subst (line old new flag)  
  (let ((index (string-match line old))
        (old-length (LENGTH old)))
    (cond (index
           (let ((new-line
                  (CONCATENATE
                   'STRING 
                   (SUBSEQ line 0 index)
                   new
                   (string-subst
                    (SUBSEQ line
                            (+ index old-length))
                    old new flag))))
             (cond (flag
                    (format T "~%old: ~S~%" line)
                    (format T "new: ~S~%" new-line)))
             new-line))
          (else line))))

(defun string-match (string pat)  
  (cond ((and (string= "" string)
              (string= "" pat))
         0)
        ((or  (string= "" string)
              (string= "" pat)
              (< (length string)
                 (length pat)))
         nil)
        ((string= (subseq string 0 (length pat))
                  pat)
         0)
        (else
         (string-match2 string pat))))

(defun string-match2 (string pat)
  (let ((index (string-match (subseq string 1) pat)))
    (if index
      (+ 1 index)
      nil)))


(defun compare-file (file-1 file-2)  
  (let ((stream-1 (open file-1 :direction :input))
        (stream-2 (open file-2 :direction :input)))
    (stream-compare stream-1 stream-2)
    (close stream-1)
    (close stream-2)
    '*end-of-file-compare*))

(defun stream-compare (s-1 s-2)  
  (cond ((or (not (input-stream-p s-1))
             (not (input-stream-p s-2)))
         (write-string 
          "Foul stream in (STREAM-COMPARE)" *error-output*))
        ((and (eof? (peek-char nil s-1 nil eof))
              (eof? (peek-char nil s-2 nil eof)))
         '*END-OF-STREAM-COMPARE*)
        ((eof? (peek-char nil s-1 nil eof))
         (format t "Stream 1 ended before stream 2"))
        ((eof? (peek-char nil s-2 nil eof))
         (format t "Stream 2 ended before stream 1"))
        (else
         (let ((l-1 (read-line s-1))
               (l-2 (read-line s-2)))
           (cond ((not (STRING= l-1 l-2))
                  (format T "~%1: ~S~%" l-1)
                  (format T "2: ~S~%" l-2))
                 (else nil))
           (stream-compare s-1 s-2)))))


;;;;;;;;; new code not in book

;;  (center-fill "Big Deal" #\* so)
;;   =>
;;  *****************************    Big Deal    *******************************

(defun center-fill (text fill-char stream)  
  (let* ((margin 4)
         (indentation (- (TRUNCATE (- (line-length stream)
                                      (LENGTH text))
                                   2)
                         margin)))
    (TERPRI stream)
    (repeat-char stream fill-char indentation)
    (write-spaces stream margin)
    (write-string text stream)
    (write-spaces stream margin)
    (repeat-char stream fill-char indentation)
    (VALUES)))

(defun char-fill (stream char)  
  (cond ((> (hpos stream)
            (line-length stream))
         (VALUES))
        (else
         (write-char char stream)
         (char-fill stream char))))


(defun star-fill (stream)  
  (char-fill stream #\*))

(defun star-headline (text stream)  
  (center-fill text #\* stream))

;; lots of bogus code for getting boundary conditions correct

(defun border-center (text border-char margin stream)  
  (let* ((indentation (- (TRUNCATE (- (line-length stream)
                                      (LENGTH text))
                                   2)
                         margin)))
    (if (ODDP (LENGTH text))
      (INCF indentation))
    (TERPRI stream)
    (repeat-char stream border-char margin)
    (write-spaces stream indentation)
    (write-string text stream)
    (if (EVENP (LENGTH text))
      (INCF indentation))
;;      (write-spaces stream indentation)  ;; ??? doesn't work, but next line does?
    (repeat-char stream #\space indentation)
    (repeat-char stream border-char margin)
    (VALUES)))


(defun center-box (stream text)  
  (TERPRI stream)
  (let ((border-width 1)
        (border-char #\*))
    (char-fill stream border-char)
    (if (LISTP text)
        (MAPC 
         #'(LAMBDA (textline)
           (border-center textline border-char border-width stream))
         text)
        (border-center text border-char border-width stream))
    (TERPRI stream)
    (char-fill stream border-char)
    (TERPRI stream)
    (VALUES)
    ))


(defun border-indent (text border-char margin stream)  
    (let* ((indentation (- (+ 1 (line-length stream))
                           margin)))
      (TERPRI stream)
      (repeat-char stream border-char margin)
      (repeat-char stream #\space margin)
      (write-string text stream)
      (tab-to-column indentation stream)
      (repeat-char stream border-char margin)
      (VALUES)))




(defun indent-box (stream text)  
  (TERPRI stream)
  (let ((border-width 1)
        (border-char #\*))
    (char-fill stream border-char)
    (if (LISTP text)
        (MAPC 
         #'(LAMBDA (textline)
           (border-indent textline border-char border-width stream))
         text)
        (border-indent text border-char border-width stream))
    (TERPRI stream)
    (char-fill stream border-char)
    (TERPRI stream)
    (VALUES)
    ))


(defun paragraph (string-list stream)  
  (let ((left 0)
        (right 60))
    (labels ((paragraph2 (string-list stream)
               (cond ((NULL string-list) nil)
                     (else
                      (sub-paragraph (capitalize-one (car string-list)) stream left right)
                      (paragraph2 (cdr string-list) stream)))))
;;    (format stream "~%\\hinge{}")    ;;
      (fresh-line stream)
      (if (STRINGP string-list)
        (sub-paragraph  (capitalize-one string-list) stream left right)
        (paragraph2 string-list stream))
      (fresh-line stream)
      (values))))

(defun sub-paragraph (str stream left right)  
  (cond ((string= "" str)
         (repeat-char stream #\space 2))
        ((> left (hpos stream))
         (tab-to-column left stream)
         (sub-paragraph str stream left right))
        ((and (< right (hpos stream))
              (whitespace? (char str 0)))
         (tab-to-column left stream)
         (sub-paragraph (subseq str 1) stream left right))
        (else
         (write-char (char str 0) stream)
         (sub-paragraph (subseq str 1) stream left right))))

(defun whitespace? (char)
  (and (characterp char)
       (standard-char-p char)
       (not (alphanumericp char))))

