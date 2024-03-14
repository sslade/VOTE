;; see ~/t/utils/date.t  
;; structures: pp 468
;; time pp 702
;; multiple-values pp 182

;; date class

;; use EVAL-WHEN so that defconstants (below) will compile correctly

(defclass date ()
  ((day
    :accessor date-day
    :initarg :day
    :type integer
    :initform 0)
   (month
    :accessor date-month
    :initarg :month
    :type integer
    :initform 0)
   (year
    :accessor date-year
    :initarg :year
    :type integer
    :initform 0)
   (symbol
    :accessor date-symbol
    :initarg :symbol
    :type symbol
    :initform nil))
  (:documentation "A class for date objects."))
 
;; 11/5/15 add find-class
(defun make-date (&rest initargs)
  (apply #'make-instance (find-class 'date) initargs))

;; 11/5/2015
;;(defmethod make-load-form ((d date))
;;  (make-load-form-saving-slots d '(day month year)))

(defmethod all-the-slots ((class date))
  '(day month year symbol))

(defmethod print-object ((d date) stream)
  (format stream "#{Date (~A) ~A/~A/~A}"
		(object-hash d)
		(date-month d)
		(date-day d)
		(date-year d)))


(defmethod print-readable ((d date) stream)
  (format stream "(list->date '(~A ~A ~A))"
		(date-month d)
		(date-day d)
		(date-year d)))


(defmethod pretty-print ((self date) stream)
    (format stream "~A, ~A ~A, ~A"
            (date_weekday_name self)
            (date_month_name self)
            (date-day self)
            (date-year self)))

(defmethod id ((d date))
  (cond ((date-symbol d))
        (else
         (setf (date-symbol d)
               (INTERN
                (format nil "DATE:~A/~A/~A"
                        (date-month d)
                        (date-day d)
                        (mod (date-year d) 100)))))))


;; 9/9/16 moved to ../load_utils.lisp
;(define-predicate date)

;---------------------------------------------------
;   Standard date durations
;---------------------------------------------------

;; 11/5/2015 - changed from defconstant to defvar

(defvar date_empty_date (make-date))
(defvar date_one_day (make-date :day 1))
(defvar date_one_week (make-date :day 7))
(defvar date_one_month (make-date :month 1))
(defvar date_one_year (make-date :year 1))


;---------------------------------------------------
;   Date system call: get-decoded-time
;---------------------------------------------------

(defun current_date ()
  (multiple-value-bind
   (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
   (get-decoded-time)
   (declare (ignore second minute hour day-of-week daylight-saving-time-p time-zone))
   (make-date
    :day date
    :month month
    :year year
   ))
)


;---------------------------------------------------
;   Date printing routines
;---------------------------------------------------

(defmethod print-header ((self date) stream &rest args)
  (declare (ignore args))
  (format stream "~A/~A/~A" 
          (date-month self)
          (date-day self)
          (date-year self)))

;---------------------------------------------------
;   Days of the week
;---------------------------------------------------

;;  > (weekday->weekday_name 3)
;;  "Thursday"
;---------------------------------------------------
(defun weekday->weekday_name (n)
  (aref '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
        n))

;;  > (date_weekday_name (current_date))
;;  "Thursday"
;---------------------------------------------------
(defun date_weekday_name (date)
  (weekday->weekday_name (get-weekday date)))

;;  > (date_weekday_symbol (current_date))
;;  THURSDAY
;---------------------------------------------------
(defun date_weekday_symbol (date)
    (INTERN (string-upcase (date_weekday_name date))))

;;  > (date_short_weekday_name (current_date))
;;  "Thu"
;---------------------------------------------------
(defun date_short_weekday_name (date)
  (SUBSEQ (date_weekday_name date) 0 3))

;;  > (get-weekday (current_date))
;;  4
;;  see page 704
;; Note: weekday numbering is off by one from Zeller, where Sunday is 0
;----------------------------------------------------------------------
(defun get-weekday (date)
  (nth-value
   6
   (decode-universal-time 
    (encode-universal-time 1 1 1
                           (date-day date)
                           (date-month date)
                           (date-year date)))
   ))


(defun leap-year? (year)  
  (cond ((zerop (rem year 4))
         (cond ((zerop (rem year 100))
                (cond ((zerop (rem year 400))
                       t)
                      (else nil)))
               (else T)))
        (else nil)))

;---------------------------------------------------
;   Months of the year
;---------------------------------------------------

;;  > (month->month_name 1)
;;  "January"
;---------------------------------------------------
(defun month->month_name (n)  
  (aref '#("January" "February" "March" "April" "May" "June"
                     "July" "August" "September" "October" "November" "December")
        (1- n)))

;;  > (date_month_name (current_date))
;;  "January"
;---------------------------------------------------
(defun date_month_name (date)  
  (month->month_name (date-month date)))

;;  > (date_short_month_name (current_date))
;;  "Jan"
;---------------------------------------------------
(defun date_short_month_name (date)  
  (subseq (date_month_name date) 0 3))


;---------------------------------------------------
;   Date arithmetic routines 
;---------------------------------------------------

;;  > (date-add (today) date_one_day)
;;  #{Date (3) 1987/1/31}
;---------------------------------------------------
(defun date-add (date1 date2)  
  (let ((new-date (copy-instance date1)))
    (add-years
     (add-months
      (add-days new-date (date-day date2))
      (date-month date2))
     (date-year date2))))

;;  > (add-days (today) 3)
;;  #{Date (4) 1987/2/2}
;;  > (add-days (today) -40)
;;  #{Date (9) 1986/12/21}
;---------------------------------------------------
(defun add-days (new-date days)  
  (setf (date-day new-date)
	   (+ (date-day new-date) days))
  (normalize-date new-date))

;;  > x
;;  #{Date (12) 1987/1/400}
;;  > (normalize-date x)
;;  #{Date (12) 1988/2/4}
;---------------------------------------------------
(defun normalize-date (new-date)  
  (let ((days (date-day new-date))
        (month (date-month new-date)))
    (cond ((> month 12)
           (INCF (date-year new-date))
           (setf (date-month new-date) (- (date-month new-date) 12))
           (normalize-date new-date))
          ((not (PLUSP month))
           (DECF (date-year new-date))
           (setf (date-month new-date) (+ 12 (date-month new-date)))
           (normalize-date new-date))
          (else
           (let ((month-total (days-per-month new-date)))
             (cond ((> days month-total)
                    (INCF (date-month new-date))
                    (setf (date-day new-date) (- days month-total))
                    (normalize-date new-date))
                   ((not (PLUSP days))
				(DECF (date-month new-date))
                    (setf (date-day new-date)
					 (+ days (days-per-month new-date)))
                    (normalize-date new-date))
                   (else new-date)))))))

;;  > (days-per-month x)
;;  29
;---------------------------------------------------
(defun days-per-month (date)  
  (days-per-month-aux (date-month date) (date-year date)))

(defun days-per-month-aux (m y)  
  (let ((month (mod m 12))
        (year (if (PLUSP m) y (1- y))))
    (if (ZEROP m) (setq m 12))
    (cond ((and (= month 2)
                (leap-year? year))
           29)
          (else
           (case month
                 ((2) 28)
                 ((4 6 9 11) 30)
                 (otherwise 31))))))

;;  > (add-months (today) 3)
;;  #{Date (14) 1987/4/30}
;---------------------------------------------------
(defun add-months (new-date months)  
  (let* ((month-total (+ (date-month new-date) months))
         (new-month (mod month-total 12))
         (new-year (TRUNCATE month-total 12)))
    (setf (date-month new-date) (if (ZEROP new-month) 12 new-month))
    (cond ((not (PLUSP month-total))
           (DECF new-year)))
    (if (ZEROP new-year)
        nil
      (setf (date-year new-date) (+ new-year (date-year new-date))))
    new-date))

;;  > (add-years (today) 3)
;;  #{Date (15) 1990/1/30}
;---------------------------------------------------
(defun add-years (new-date years)  
  (setf (date-year new-date)
	   (+ (date-year new-date) years))
  new-date)


;;  > (date-subtract (today) date_one_month)
;;  #{Date (16) 1986/12/30}
;---------------------------------------------------
(defun date-subtract (date1 date2)  
  (let ((new-date (copy-instance date1)))
    (add-years
     (add-months
      (add-days new-date (- (date-day date2)))
      (- (date-month date2)))
     (- (date-year date2)))))


;;  > (days-between (yesterday) (tomorrow))
;;  2
;---------------------------------------------------
(defun days-between (date1 date2)  
  (let ((start-year (if (date-before? date1 date2)
                        (date-year date1)
                        (date-year date2))))
    (abs (- (julian-date date1 start-year)
            (julian-date date2 start-year)))))

;;; julian-date     returns the number of days since 1 Jan of start_year
;;;                 for the given date
;;
;;; year-count      returns the number of days in all the years between
;;;                 the given year and start-year
;;  > (julian-date (today) 1981)
;;  2221
;---------------------------------------------------
(defun julian-date (date start_year)  
  (labels ((year-count (year start-year)
             (cond ((< year start-year) 0)
                   (else
                    (+ (year-count (1- year) start-year)
                       (if (leap-year? year) 366 365)))))
           (month-count (month year)
             (cond ((<= month 0) 0)
                   (else (+ (month-count (1- month) year)
                            (days-per-month-aux month year))))))
    (+ (year-count (1- (date-year date)) 
                   start_year)
       (month-count (1- (date-month date))
                    (date-year date))
       (date-day date))))



;---------------------------------------------------
;   Dynamic dates 
;---------------------------------------------------

;;(defvar date-table (my-make-table date-table) 
;;  "Hash table to hold standard date objects.")

(defvar date-table (make-hash-table))

;;(make-executable date-table)
(setf (symbol-function 'date-table)
      #'(lambda (key) (gethash key date-table)))
(defun (setf date-table) (val key) 
  (setf (gethash key date-table) val))

(defun yesterday ()
  (cond ((gethash 'yesterday date-table))
        (else (date-subtract (today) date_one_day))))

(defun today ()
  (cond ((gethash 'today date-table))
        (else (current_date))))

(defun tomorrow ()
  (cond ((gethash 'tomorrow date-table))
        (else (date-add (today) date_one_day))))

(defun init-date-table ()
;;  (setq date-table (make-hash-table))
;;  (setq date-table (my-make-table date-table))
  (update-week-in-date-table)
  date-table)

;; Updates the dynamic dates in the date table.
(defun update-week-in-date-table ()
  (mapc
   #'(lambda (day)
       (setf (gethash day date-table) (intern-date (list day))))
   '(today yesterday tomorrow weekend
     monday tuesday wednesday thursday friday saturday sunday)))


;;  > (intern-date '(4 27 87))
;;  #{Date (10) 4/27/1987}
;;      Checks to see if a date is already in the date table. 
;;      If so, returns that date; otherwise, enters it.
;---------------------------------------------------
(defun intern-date (date)  
  (let ((date-id (cond ((date? date) 
                        (id date))
                       (else
                        (setq date (list->date date))
                        (id date)))))
    (cond ((gethash date-id date-table))
          (else
           (setf (gethash date-id date-table) date)))))

;---------------------------------------------------
;   Date comparison predicates
;---------------------------------------------------

;;  > (date-equal? (today) (yesterday))
;;  ()
;---------------------------------------------------
(defun date-equal? (date1 date2)  
  (normalize-date date1)
  (normalize-date date2)
  (and (= (date-day date1)
          (date-day date2))
       (= (date-month date1)
          (date-month date2))
       (= (date-year date1)
          (date-year date2))))

;;  > (date-before? (yesterday) (today))
;;  #T
;---------------------------------------------------
(defun date-before? (date1 date2)  
  (normalize-date date1)
  (normalize-date date2)
  (or (< (date-year date1) (date-year date2))
      (and (= (date-year date1) (date-year date2))
           (or (< (date-month date1) (date-month date2))
               (and (= (date-month date1) (date-month date2))
                    (< (date-day date1) (date-day date2)))))))

;---------------------------------------------------
;   Date parsing routines
;---------------------------------------------------

; dates:
;   ((1 23 87)
;    (1 23 1987)
;    (1987 4 23)
;    (1 23)
;    (jan 23)
;    (next monday)
;    (last monday)
;    (weekend)
;    (yesterday)
;; durations: [interpretted as (today) + duration]
;    (2 days)
;    (2 weeks)
;    (1 month ago)))


;;  > (list->date '(1 week))
;;  #{Date (295) 2/7/1987}
;;  > (list->date '(2 5))
;;  #{Date (296) 2/5/1987}
;---------------------------------------------------
(defun list->date (l &rest nd)  
    (let ((new-date (if nd (car nd) (current_date))))
        (cond ((some #'duration? l)
               (add-duration new-date l 0))
              ((some #'weekday? l)
               (set-weekday new-date l))
              ((isa-date? l)
               (convert-date new-date l))
              ((dynamic-date-check l new-date))
              (else (error "bad data in LIST->DATE: ~A" l)))))

;;  > (dynamic-date-check '(yesterday) (list->date '(5 2 52)))
;;  #{Date (9) 5/1/1952}
;---------------------------------------------------
(defun dynamic-date-check (l today-date)  
  (cond ((NULL l) today-date)
        (else
         (case (car l)
           ((today) today-date)
           ((tomorrow) (add-days today-date 1))
           ((yesterday) (add-days today-date -1))
           (otherwise (dynamic-date-check (cdr l) today-date))))))


;;  > (weekday->number 'fri)
;;  4 (using LISP numbering scheme, different from Zeller)
;---------------------------------------------------
(defun weekday->number (x)  
  (if (stringp x) (setq x (intern (string-upcase x))))
  (case x
    ((sun sunday)  6)
    ((mon monday)  0)
    ((tue tuesday) 1)
    ((wed wednesday) 2)
    ((thu thursday) 3)
    ((fri friday) 4)
    ((sat saturday weekend) 5)
    (otherwise   nil)))

;;  >  (set-weekday (today) '(friday last))
;;  #{Date (305) 1/30/1987}
;---------------------------------------------------
(defun set-weekday (new-date l)  
  (labels ((next-date (d1 d2)
             (if (member 'last l) (date-subtract d1 d2) (date-add d1 d2)))
           (find-weekday (l)
             (cond ((null l) nil)
                   ((weekday? (car l)) (weekday->number (car l)))
                   (else (find-weekday (cdr l)))))
           (check-date (date weekday)
             (cond ((= weekday (get-weekday date)) date)
                   (else
                    (check-date (next-date date date_one_day) weekday)))))
    (check-date (next-date new-date date_one_day) (find-weekday l))))

;;  > (isa-date? '(1987 1 21))
;;  #T
;---------------------------------------------------
(defun isa-date? (l)  
  (and (listp l) ;; **PROPER-LIST**
       (> (length l) 1)
       (< (length l) 4)
       (some #'day? l)
       (some #'month? l)
       (if (= (length l) 3) 
         (some #'year? l))))


;;  > (convert-date (today) '(5 2 52))
;;  #{Date (294) 5/2/1952}
;---------------------------------------------------
(defun convert-date (new-date l)  
  (cond ((null l) new-date)
        ((year? (car l))
         (setf (date-year new-date) 
               (if (< (car l) 100) (+ 1900 (car l)) (car l)))
         (convert-date new-date (cdr l)))
        ((month? (car l))
         (setf (date-month new-date) (month->number (car l)))
         (if (day? (cadr l))
           (progn (setf (date-day new-date) (cadr l))
                  (convert-date new-date (cddr l)))
           (convert-date new-date (cdr l))))
        ((day? (car l))
         (setf (date-day new-date) (car l))
         (convert-date new-date (cdr l)))))


;;  > (add-duration (today) '(3 days) 0)
;;  #{Date (291) 2/3/1987}
;---------------------------------------------------
(defun add-duration (new-date l count)  
  (cond ((numberp (car l))
         (add-duration new-date (cdr l) (+ count (car l))))
        ((word->number (car l))
         (add-duration new-date (cdr l) (+ count (word->number (car l)))))
        ((duration? (car l))
         (let ((dur-date (make-date))
               (dur-list (duration->numbers (car l))))
           (setf (date-day dur-date) (* count (pop dur-list)))
           (setf (date-month dur-date) (* count (pop dur-list)))
           (setf (date-year dur-date) (* count (pop dur-list)))
           (if (member 'ago l) 
             (date-subtract new-date dur-date)
             (date-add new-date dur-date))))))

;;  > (word->number 'one)
;;  1
;---------------------------------------------------
(defun word->number (x)  
  (if (STRINGP x) (setq x (INTERN (string-upcase x))))
  (case x
    ((one) 1)
    ((two) 2)
    ((three) 3)
    ((four) 4)
    ((five) 5)
    ((six) 6)
    ((seven) 7)
    ((eight) 8)
    ((nine) 9)
    ((ten) 10)
    ((twenty) 20)
    (otherwise nil)))

;;  > (weekday? 'fri)
;;  #T
;---------------------------------------------------
(defun weekday? (x)  
  (if (stringp x) (setq x (intern (string-upcase x))))
  (member x '(mon monday tue tuesday wed wednesday thu thursday
              fri friday sat saturday sun sunday weekend)))


;;  a year is an integer x, such that   31 < x < 100
;;                                 or 1900 < x < 2000
;;  > (year? 1988)
;;  #T
;---------------------------------------------------
(defun year? (x)  
  (and (integerp x)
       (> x 31)
       (< x 2100)
       (or (> x 1900)
           (< x 100))))

;;  minimal requirements for day-ness
;;  > (day? 23)
;;  #T
;;  > (day? 40)
;;  ()
;---------------------------------------------------
(defun day? (x)  
  (and (integerp x)
       (<= x 31)
       (plusp x)))

;;  > (month? "jan")
;;  #T
;---------------------------------------------------
(defun month? (x)  
  (if (stringp x) (setq x (intern (string-upcase x))))
  (or (and (integerp x)
           (plusp x)
           (<= x 13))
      (member x 
              '(jan january feb february mar march apr april may jun june
                jul july aug august sep september oct october nov november
                dec december))))

;;  > (month->number 'dec)
;;  12
;---------------------------------------------------
(defun month->number (x)  
  (if (STRINGP x) (setq x (INTERN (string-upcase x))))
  (if (NUMBERP x) 
    x
    (case x
      ((jan january)  1)
      ((feb february) 2)
      ((mar march)    3)
      ((apr april)    4)
      ((may)          5)
      ((jun june)     6)
      ((jul july)     7)
      ((aug august)   8)
      ((sep september) 9)
      ((oct october)  10)
      ((nov november) 11)
      ((dec december) 12)
      (otherwise      nil))))

;;  > (duration? 'day)
;;  #T
;---------------------------------------------------
(defun duration? (x)  
  (if (stringp x) (setq x (intern (string-upcase x))))
  (member x '(hour hours day days week weeks month months year years)))


;;  > (duration->numbers 'weeks)
;;  (7 0 0)
;---------------------------------------------------
(defun duration->numbers (x)  
  (case x
    ((day days)    '(1 0 0))
    ((week weeks)  '(7 0 0))
    ((month months) '(0 1 0))
    ((year years)  '(0 0 1))
    (otherwise          nil)))


