;; time class

;; use EVAL-WHEN so that defconstants (below) will compile correctly

(defclass ctime ()
  ((hour
    :accessor ctime-hour
    :initarg :hour
    :type integer
    :initform 0)
   (minute
    :accessor ctime-minute
    :initarg :minute
    :type integer
    :initform 0)
   (second
    :accessor ctime-second
    :initarg :second
    :type integer
    :initform 0)
   (daylight-saving
    :accessor ctime-daylight-saving
    :initarg :daylight-saving
    :type symbol
    :initform nil)
   (zone
    :accessor ctime-zone
    :initarg :zone
    :type integer
    :initform 0)
   (symbol
    :accessor ctime-symbol
    :initarg :symbol
    :type symbol
    :initform nil))
  (:documentation "A class for time objects."))

; (define-predicate ctime)

;; (make-ctime :hour 9 :minute 15)
;;----------------------------------------------
(defun make-ctime (&rest initargs)
  (apply #'make-instance 'ctime initargs ))

(defmethod all-the-slots ((class ctime))
  '(hour minute second daylight-saving zone symbol))


(defmethod print-object ((ct ctime) stream)
  (format stream "#{CTime (~A) ~A}"
		(object-hash ct)
        	(->hms
		 (ctime-hour ct)
		 (ctime-minute ct)
		 (ctime-second ct))))


(defmethod print-readable ((ct ctime) stream)
  (format stream "(list->time ~A ~A ~A)"
		 (ctime-hour ct)
		 (ctime-minute ct)
		 (ctime-second ct)))

(defmethod pretty-print ((self ctime) stream)
  (format stream "~A ~A"
		(time_normal_time self)
		(time_meridiem_name self))
  (if (and (numberp (ctime-zone self))
		 (zerop (ctime-zone self)))
	 nil
    (format stream " (~A)"
		  (timezone->string (ctime-daylight-saving self)
						(ctime-zone self)))))




(defmethod id ((time ctime))
  (cond ((ctime-symbol time))
	   (else
	    (setf (ctime-symbol time)
			(INTERN
			 (format nil "CTIME-~A"
				    (time_military_time time)))))))


;---------------------------------------------------
;   Time printing routines
;---------------------------------------------------

(defmethod print-header ((self ctime) stream &rest args)
  (declare (ignore args))
  (format stream "~A" 
          (time_military_time self)))
         

;---------------------------------------------------
;   Standard time durations
;---------------------------------------------------

(defconstant time_empty_time (make-ctime))  
(defconstant time_one_second (make-ctime :second 1))
(defconstant time_one_minute (make-ctime :minute 1))
(defconstant time_five_minutes  (make-ctime :minute 5))
(defconstant time_fifteen_minutes  (make-ctime :minute 15))
(defconstant time_thirty_minutes  (make-ctime :minute 30))
(defconstant time_one_hour  (make-ctime :hour 1))
(defconstant time_noon  (make-ctime :hour 12))
(defconstant time_midnight (make-ctime))  

;---------------------------------------------------
;   Time system calls
;---------------------------------------------------

;;  > (current_time)
;;  #{Time (17) 12:07 (EST)}
;---------------------------------------------------
(defun current_time ()
  (multiple-value-bind
   (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
   (get-decoded-time)
   (declare (ignore date month year day-of-week))
   (make-ctime
    :hour hour
    :minute minute
    :second second
    :daylight-saving daylight-saving-time-p
    :zone time-zone
   ))
)

;;  > (current_timezone)
;;  "EST"
;---------------------------------------------------
(defun current_timezone ()
  (let* ((time (current_time))
	    (zone (ctime-zone time))
	    (dst  (ctime-daylight-saving time)))
    (timezone->string dst zone)))
    
(defun timezone->string (dst zone)
  (if dst
	 (aref '#("tz0D" "GMTD" "tz2D" "tz3D" "ADT" "EDT" "CDT" "MDT" "PDT") zone)
      (aref '#("tz0" "GMT" "tz2" "tz3" "AST" "EST" "CST" "MST" "PST") zone)))


;---------------------------------------------------
;   Time printing routines
;---------------------------------------------------

;;  > (time_meridiem_name (current_time))
;;  "pm"
;---------------------------------------------------
(defun time_meridiem_name (time)  
  (if (< (ctime-hour time) 12) "am" "pm"))

;;  > (number->string 123 4)
;;  "0123"
;---------------------------------------------------
(defun number->string (n w)
  (let ((f (format nil "~A~D~A" "~" w ",'0D")))
    (format nil f n)))

;;  > (time_short_normal_time (current_time))
;;  " 7:24"
;---------------------------------------------------
(defun time_short_normal_time (time)  
  (let* ((hour (ctime-hour time))
         (minute (ctime-minute time))
         (two-digit-time
                (or (>= (mod hour 12) 10)
                    (zerop (mod hour 12)))))
    (concatenate 'string
			  (if two-digit-time "" " ")
			  (->hm hour minute))))

;;  > (->hm 7 3)
;;  "7:03"
;---------------------------------------------------
(defun ->hm (hour minute)  
  (format nil "~D:~2,'0D" 
		(cond ((zerop hour) 12)
			 ((<= hour 12) hour)
			 (t (- hour 12)))
		minute))

;;  > (->hms 14 20 0)
;;  "2:20:00"
;---------------------------------------------------
(defun ->hms (hour minute second)  
  (format nil "~D:~2,'0D:~2,'0D" 
		(cond ((zerop hour) 12)
			 ((<= hour 12) hour)
			 (t (- hour 12)))
		minute
		second))

;;  > (time_civilian_time (current_time))
;;  "12:10:36"
;---------------------------------------------------
(defun time_civilian_time (time)  
  (->hms (ctime-hour time)
         (ctime-minute time)
         (ctime-second time)))

;;  > (time_normal_time (current_time))
;;  "12:10:57"
;---------------------------------------------------
(defun time_normal_time (time)  
  (let ((hour (ctime-hour time))
        (minute (ctime-minute time))
        (second (ctime-second time)))
    (cond ((and (zerop minute) (zerop second)
			 (or (zerop hour) (= hour 12)))
           (if (zerop hour) "Midnight" "Noon"))
          (else (->hms hour minute second)))))

;;  > (time_military_time (current_time))
;;  "12:11"
;---------------------------------------------------
(defun time_military_time (time)  
  (let ((hour (ctime-hour time))
        (minute (ctime-minute time)))
    (format nil "~A:~A"
		  (number->string hour 2)
		  (number->string minute 2))))


;; print out a date/time stamp on a given stream.

(defun time-stamp (stream)  
  (format stream "~%;; As of:  ")
  (pretty-print (current_time) stream)  
  (format stream ".  ")
  (pretty-print (current_date) stream)
  (format stream "~%")
  )

;---------------------------------------------------
;   Time arithmetic routines
;---------------------------------------------------

;;  > (time-add (current_time) time_one_hour)
;;  #{Time (18) 13:12 (EST)}
;---------------------------------------------------
(defun time-add (time1 time2)  
  (let ((new-time (copy-instance time1)))
    (setf (ctime-second new-time)
         (+ (ctime-second new-time)
            (ctime-second time2)))
    (setf (ctime-minute new-time)
         (+ (ctime-minute new-time)
            (ctime-minute time2)))
    (setf (ctime-hour new-time)
         (+ (ctime-hour new-time)
            (ctime-hour time2)))
    (normalize-time new-time)))

;;  > x
;;  #{Time (20) 12:80 (EST)}
;;  > (normalize-time x)
;;  #{Time (20) 13:20 (EST)}
;---------------------------------------------------
(defun normalize-time (new-time)  
  (let ((seconds (ctime-second new-time))
        (minutes (ctime-minute new-time))
        (hours   (ctime-hour new-time)))
    (setf (ctime-second new-time) (mod seconds 60))
    (setq minutes 
         (+ minutes (TRUNCATE seconds 60) (if (MINUSP seconds) -1 0)))
    (setf (ctime-minute new-time) (mod minutes 60))
    (setf (ctime-hour new-time) 
         (+ hours (TRUNCATE minutes 60) (if (MINUSP minutes) -1 0)))
    new-time))

;;  > (time-subtract (current_time) time_one_hour)
;;  #{Time (21) 11:14 (EST)}
;---------------------------------------------------
(defun time-subtract (time1 time2)  
  (let ((new-time (copy-instance time1)))
    (setf (ctime-second new-time)
         (- (ctime-second new-time)
            (ctime-second time2)))
    (setf (ctime-minute new-time)
         (- (ctime-minute new-time)
            (ctime-minute time2)))
    (setf (ctime-hour new-time)
         (- (ctime-hour new-time)
            (ctime-hour time2)))
    (normalize-time new-time)))

;---------------------------------------------------
;   Time comparison routines
;---------------------------------------------------

;;  These are direct translations of the date comparison routines above.

;;  > (time-equal? (current_time) (current_time))
;;  #T
;---------------------------------------------------
(defun time-equal? (time1 time2)  
  (normalize-time time1)
  (normalize-time time2)
  (and (= (ctime-second time1)
          (ctime-second time2))
       (= (ctime-minute time1)
          (ctime-minute time2))
       (= (ctime-hour time1)
          (ctime-hour time2))))

;;  > (time-before? (current_time) (current_time))
;;  ()
;---------------------------------------------------
(defun time-before? (time1 time2)  
  (normalize-time time1)
  (normalize-time time2)
  (or (< (ctime-hour time1) (ctime-hour time2))
      (and (= (ctime-hour time1) (ctime-hour time2))
           (or (< (ctime-minute time1) (ctime-minute time2))
               (and (= (ctime-minute time1) (ctime-minute time2))
                    (< (ctime-second time1) (ctime-second time2)))))))


;---------------------------------------------------
;   Time parsing routines
;---------------------------------------------------

;; Examples:
;  ((3 20)
;   (3 20 am)
;   (11 pm)
;   (noon)
;   (midnight)
;   (now))

;---------------------------------------------------
(defun list->time (l)  
    (let ((new-time (make-ctime)))
        (cond ((isa-time? l)
               (convert-time new-time l))
              ((dynamic-time-check new-time l))
              (else
                (error "bad data in LIST->TIME_~A" l)))))


;---------------------------------------------------
(defun isa-time? (l)  
    (and (listp l)   ;; **PROPER-LIST** 
         (> (length l) 0)
         (< (length l) 5)
         (SOME #'hour? l)
         (if (> (length l) 1) 
             (SOME #'minute? l)
   		   T)
         (if (= (length l) 4) 
             (ampm? l) 
             T)))

;---------------------------------------------------
(defun hour? (x)  
    (and (INTEGERP x)
         (<= x 23)
         (>= x 0)))

;---------------------------------------------------
(defun minute? (x)  
    (and (INTEGERP x)
         (<= x 60)
         (>= x 0)))

;---------------------------------------------------
(defun ampm? (x)  
    (or (MEMBER 'am x)
        (memq 'pm x)))


;---------------------------------------------------
(defun convert-time (new-time l)  
    (if (hour? (car l))
        (setf (ctime-hour new-time) (normalize-hours (car l))))
    (convert-time-aux new-time (cdr l)))
                                                             
;;  Assume afternoon for hours 1 through 7, morning for 8 through 12.
;---------------------------------------------------
(defun normalize-hours (hour)  
    (if (< hour 8) 
        (+ hour 12)
        hour))

;---------------------------------------------------
(defun convert-time-aux (new-time l)  
    (cond ((NULL l) new-time)
          ((and (minute? (car l))
                (ZEROP (ctime-minute new-time)))
           (setf (ctime-minute new-time) (car l))
           (convert-time-aux new-time (cdr l)))
          ((minute? (car l))
           (setf (ctime-second new-time) (car l))
           (convert-time-aux new-time (cdr l)))
          ((EQ (car l) 'am)
           (setf (ctime-hour new-time) (mod (ctime-hour new-time) 12))
           (convert-time-aux new-time (cdr l)))
          ((EQ (car l) 'pm)
           (setf (ctime-hour new-time) (+ 12 (mod (ctime-hour new-time) 12)))
           (convert-time-aux new-time (cdr l)))
          (else 
		 (error "bad data in CONVERT-TIME-AUX: ~A" l))))

;---------------------------------------------------
(defun dynamic-time-check (new-time l)  
    (cond ((NULL l) new-time)
          (else
            (case (car l)
             ((noon) (setf (ctime-hour new-time) 12) 
                     new-time)
             ((midnight) new-time)
             ((now) (current_time))
             (otherwise (dynamic-date-check (cdr l) new-time))))))

