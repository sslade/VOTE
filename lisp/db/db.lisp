
;---------------------------------------------------
;   Contents Summary
;---------------------------------------------------

;;  Required fields for record types:
;;
;;      index
;;      current-flag
;;      status
;;      db
;;      date-open
;;      date-closed
;;      keywords    ??
;;
;;  Required operations for record types:
;;
;;      inorder?
;;      print-header
;;      pretty-print
;;      print-readable
;;      input-prototype
;;      update-prototype
;;      no-print-readable-fields
;;      no-pp-fields
;;      table-index-fields


;;----------------------------------------------------------
;;      Global variables
;;----------------------------------------------------------
;;
;;      *current-db*      db subdirectory, as it were
;;      *command-list*    list of db commands
;;      *undo-list*       stack of undo procedures
;;      *db-table*        table of databases, indexed by name
;;
;;
;;----------------------------------------------------------
;;      The database symbols
;;----------------------------------------------------------

(defvar *current-db*)

(defvar *command-list*)
(defvar *command-alist*)
(defvar *undo-list* nil)
(setf *db-table* (make-hash-table))
; (make-executable *db-table*)
(setf (symbol-function '*db-table*)
      #'(lambda (key) (gethash key *db-table*)))
(defun (setf *db-table*) (val key) 
  (setf (gethash key *db-table*) val))




(defvar task)


;;----------------------------------------------------------
;;      Top Level Driver Routines
;;----------------------------------------------------------

(defun dbase (&rest args)  
  (setf *current-db* (if (car args) (car args) task))
  (initialize_dbase)
  (loop
      (if (member (process_command) '(exit quit))
        (return)))
  'end-of-dbase)

;; for compatibility with the old documentation
;; 5/16/2018 caused error on load
;; (function-synonym tdb dbase)
(setf (symbol-function 'tdb) (symbol-function 'dbase))

(defun initialize_dbase ()
  (format t "~%Initializing database system.~%")
  (init-date-table)
  (init-command-list)
;;  (read-line *STANDARD-INPUT*)    ;; <<-- clear buffer...
  )

(defun process_command ()
  (let* ((update-mark (cond ((db-update-list (find-root-db *current-db*))
                             "*...")
                            ((db-all *current-db*)
                             "....")
                            (else
                             "%%%%")))
         (prompt (concatenate 'string update-mark (db-prompt *current-db*)))
         (input (prompted-list-input prompt)))
    (process_command_input input *command-list*)))

(defun process_command_input (input commands)  
  (let ((input-command (car input))
        (input-args (cdr input))
        (command-alist (or *command-alist*
                           (setf *command-alist* (make-command-alist commands)))))
    ;; remove after debug
    (db-format t "input-args: ~A~%" input-args)
    (cond
     ((process_aliased_command input-command input-args commands))
  
     ((normal_command_and_undo input-command command-alist input-args))

     ((check_for_current-items_numbers input))
 
     (else
      (db-format t "checking for spelling correction and keyword")
      ;;  check for index to db item using spelling correction then synonyms
      (let* ((table (db-table *current-db*))
             (synonyms (gethash input-command (db-synonym-table (find-root-db *current-db*))))
                       
             (output-string "Unknown command.")
             (word (progn (db-format t "input-command: ~A" input-command)
                          (spell-match table input-command)))
             (words nil))
             ;; (words (and synonyms (collect synonyms table))))
        (db-format t "word: ~A ~%words: ~A" word words)
        (cond (word
               (if (eq input-command word)
                 (format t "~%Keyword index: ~A~%" word)
                 (format t "~%Keyword index with spelling correction: ~A ==> ~A~%"
                         input-command
                         word))
               (set-current-items *current-db* (gethash word table))
               (setf output-string "")
               (headers_command 'current))
              (words
               (format t "~%Keyword index with synonym~P: ~A ==> ~A~%"
                       (length synonyms)
                       input-command
                       synonyms)
               (set-current-items *current-db* (mappend 
                                                #'(lambda (w) (gethash w table))
                                                words))
               (headers_command 'current))
              (else
               (format t "~%~A" output-string))))
      nil))))

(defun process_aliased_command (input-command input-args commands)
  (let ((alias (assoc input-command (db-aliases (find-root-db *current-db*)))))
    (cond (alias
           (process_command_input
            (append (cdr alias) input-args) commands))
          (else nil))))

(defun normal_command_and_undo (input-command command-alist input-args)
  (let ((cmd (cdr (assoc input-command command-alist))))
    (cond (cmd
           (process_undo_command cmd input-args)
           (apply (command-procedure cmd) input-args)
           (command-id cmd))
          (else nil))))

;; always return nil
(defun check_for_current-items_numbers (input)
  (let ((num-list (decode-numbers input)))
    (cond (num-list
           (set-current-items *current-db* num-list)
           (headers_command 'current)))
    nil))

;;----------------------------------------------------------
;;      Item selection options
;;----------------------------------------------------------

;;  used by headers command

;;  add arguments to filter the items
;;  sample args: today, pending, active, deleted

(defun get-selection-predicate (arg-list)  
  (let ((num-list (decode-numbers arg-list)))
    (cond (num-list
           (set-current-items *current-db* num-list)
           #'(lambda (item)
               (slot-value item 'current-flag)))
          (else
           (case (car arg-list)
             ; Current/selected
             ((current select selected *)
              #'(lambda (item)
                  (slot-value item 'current-flag)))
; dates
             ((today)
              #'(lambda (item)
                  (and (date? (slot-value item 'date-due))
                       (date-equal? (slot-value item 'date-due) (today)))))
             ((week nextweek)
              #'(lambda (item)
                  (and (date? (slot-value item 'date-due))
                       (date-before? (yesterday) (slot-value item 'date-due))
                       (date-before? (slot-value item 'date-due)
                                     (date-add (today) date_one_week)))))
             ((lastweek)
              #'(lambda (item)
                  (and (date? (slot-value item 'date-due))
                       (date-before? (slot-value item 'date-due) (today))
                       (date-before? (date-subtract (today) date_one_week)
                                     (slot-value item 'date-due)))))
             ; status
             ((pending pend)
              #'(lambda (item) (pending? item)))
             
             ((deleted del)
              #'(lambda (item) (deleted? item)))
             
             ((active)
              #'(lambda (item)
                  (and (not (date? (slot-value item 'date-due)))
                       (active? item))))
             
             ((updates)
              #'(lambda (item)
                  (member item (db-update-list (find-root-db *current-db*)))))
             
             ; other
             ((last)
              #'(lambda (item)
                  (eq item (last (db-all *current-db*)))))
           
             (otherwise #'(lambda (item) 
                            (declare (ignore item))T)))))))



;; numerical cases:
;;   1     -->  (1)
;;   2-5   -->  (2 3 4 5)
;;   2,4,7 -->  (2 4 7)
;;   7 2-4 -->  (7 2 3 4)
(defun decode-numbers (arg-list)  
  (if (NULL arg-list) 
    nil
    (let ((num-list (process-number-arg (car arg-list))))
      (cond (num-list
             (append num-list (decode-numbers (cdr arg-list))))
            (else
             (decode-numbers (cdr arg-list)))))))

(defun process-number-arg (l)  
  (process-number-arg-str (format nil "~A" l)))


;;; cannot use : due to keyword readmacros in CL
(defun filter-number-arg-string (str-l)  
 (cond ((string= "" str-l) "")
       (else
        (let* ((ch (char str-l 0))
               (val (cond ((eq ch #\,)
                           " ")
                          ((eq ch #\-)
                           " - ")
                          ((eq ch #\$)
                           (format nil "~A" (1- (length (db-all *current-db*)))))
                          (else (string ch)))))
          (concatenate 'string val
                       (filter-number-arg-string (subseq str-l 1)))))))

(defun process-number-arg-str (str-l)  
  (process-number-args-list (read-string->list
                             (filter-number-arg-string str-l))))


;; won't screw up on "A-1" or "CT-3" cases
;; ** changed 1:9 syntax to 1-9 to avoid reader problem with colons -- special keyword syntax
;; ** OK 2/22/93
(defun process-number-args-list (num-l)  
  (let ((one (car num-l))
        (two (cadr num-l))
        (three (caddr num-l)))
    (cond ((null num-l) nil)
          ((and (eq two '-)
                (numberp one)
                (numberp three))
           (if (= one three)
               (process-number-args-list (cddr num-l))
               (cons one (process-number-args-list (cons (1+ one)
                                                         (cdr num-l))))))
          ((eq  one '-)
           (process-number-args-list (cddr num-l)))
          ((numberp one)
           (cons one (process-number-args-list (cdr num-l))))
          (else
           (process-number-args-list (cdr num-l))))))


;;----------------------------------------------------------
;;      General data base structure
;;----------------------------------------------------------

;;;   (define (push-update item db . old-db)
;;;     (push (db-update-list (find-root-db db)) (list item db (car old-db))))

;; revised to check to see if item is already in the update list.
;; this should make file updates a little faster.
;; To make pop-update work properly, we push NIL on the list if
;; item is already there.  This condition is checked in save_updates_command.
;(defun push-update (item selector db &optional old-db) 
;  (declare (ignore selector))
;  (let ((update (list item db old-db))
;        (root-db  (find-root-db db)))
;;;; See pol/dd.t for the revised version of push-update
;;;;    (if (and (bound? decision)
;;;;             (db-all decision)
;;;;             (memq selector (decision-dependencies item)))
;;;;        (push (dd-updates *ddd*) (cons selector item)))
;    (if (member update (db-update-list root-db) :test #'equalp)
;        (push nil (db-update-list root-db))
;        (push update (db-update-list root-db)))))



(defun pop-update (db)  
  (pop (db-update-list (find-root-db db))))


(defclass db ()
  ((table
    :documentation "key index table for db"
    :accessor db-table
    :initarg :table
    :type hash-table
    :initform nil)
   (parent                 
    :documentation "superior data base"
    :accessor db-parent
    :initarg :parent
    :type db
    :initform nil)
   (current-items          ;   pointer to list of current active items
    :accessor db-current-items
    :initarg :current-items
    :type list
    :initform nil)
   (all                    ;   list of all items in data base
    :accessor db-all
    :initarg :all
    :type list
    :initform nil)
   (commands               ;   list of commands used by data base
    :accessor db-commands
    :initarg :commands
    :type list
    :initform nil)
   (aliases                ;   command aliases                          **
    :accessor db-aliases
    :initarg :aliases
    :type list
    :initform nil)
   (class                  ;   class of data base items 
    :accessor db-class
    :initarg :struct
    :type symbol
    :initform nil)
   (prompt                 ;   prompt used for input
    :accessor db-prompt
    :initarg :prompt
    :type string
    :initform nil)
   (name                   ;   name of db
    :accessor db-name
    :initarg :name
    :type symbol
    :initform nil)
   (data-file              ;   string: filename for storing data
    :accessor db-data-file
    :initarg :data-file
    :type string
    :initform nil)
   (print-file             ;   string: filename for formatted output
    :accessor db-print-file
    :initarg :print-file
    :type string
    :initform nil)
   (compile-date           ;   date that database was last compiled     **
    :accessor db-compile-date
    :initarg :compile-date
    :type date
    :initform nil)
   (print-date             ;   date that database was last printed      **
    :accessor db-print-date
    :initarg :print-date
    :type date
    :initform nil)
   (update-list            ; items that have changed   
    :accessor db-update-list
    :initarg :update-list
    :type list
    :initform nil)
   (synonym-alist          ; alist of synonyms                          **
    :accessor db-synonym-alist
    :initarg :synonym-alist
    :type list
    :initform nil)
   (synonym-table          ; table of synonyms (inverted from alist)
    :accessor db-synonym-table
    :initarg :synonym-table
    :type hash-table
    :initform nil)
   )
  (:documentation "A class for database objects."))

(defun make-db (&rest initargs)
  (apply #'make-instance 'db initargs))

; (defpredicate db)
;(define-predicate db)
(defun db? (obj) (typep obj db))
(defun db-p (obj) (typep obj db))

(defmethod id ((d db))
  (db-name d))

(defmethod print-object ((self db) stream)
  (format stream "#{db (~A) ~A}"
        (object-hash self)
        (db-name self)))


(defmethod print-readable2 ((self db) stream)
  (format stream "~S" (id self)))


(defmethod pretty-print ((self db) stream)
  (mapc 
   #'(lambda (slot)
       (let ((value (slot-value self slot)))
         (cond ((null value)
                nil)
               ((member slot (no-pp-fields self))
                nil)
               ((eq slot 'remarks)
                (format stream "~%Remarks:")
                (mapc 
                 #'(lambda (str)
                     (format stream "~%~A" str))
                 value))
               (else
                (format stream "~%~A: ~15T"
                        (capitalize slot))
                (display value stream)))))
   (mapcar #'car (class-direct-instance-slots (class-of self))))
  (values))

(defmethod set-current-items ((self db) items)
  (flet ((set-item-current-flag (obj val) (setf (slot-value obj 'current-flag) val))
         (item? (obj) (typep obj (db-class self))))
    (if (and (atom items) items)
      (setq items (list items)))
    ;; remove flags from existing current items
    (mapc #'(lambda (item)
              (if (item? item)
                (set-item-current-flag item nil)))
          (db-current-items self))
    ;; not too efficient.  passes though list too many times.
    (setq items
          (let ((max-index (length (db-all self))))
            (mapcar #'(lambda (item)
                        (cond ((numberp item)
                               (let ((num (check-range item max-index)))
                                 (nth num (db-all self))))
                              (else item)))
                    items)))
    (mapc 
     #'(lambda (item)
         (if (item? item)
           (set-item-current-flag item t)))
     items)
    (setf (db-current-items self) items)))

(defmethod init-db ((self db))
  (set-current-items self nil)
  (setf (db-all self) nil)
  (setf (db-update-list self) nil)
  (setf (db-table self) (make-hash-table))
  (setf (db-synonym-table self) (make-hash-table))
  (setf (gethash (db-name self) *db-table*) self)
  (let ((name (string-downcase (format nil "~A" (db-name self)))))
    (setf (db-data-file self) (merge-pathnames data name))
    (setf (db-print-file self) (merge-pathnames data (format nil "~A.print" name))))
  self
)


(defun item? (obj) 
  (typep obj (db-class *current-db*)))

(defmethod insert-item ((self db) item)
  (set-current-items self item)
  (setf (db-all self) (adjoin item (db-all self)))
  (index-insert-item (db-table self) item)
;;  (mapc 
;;   #'(lambda (slot)
;;       (table-intern (db-table self) slot item))
 ;;      (setf (gethash (slot-value item slot) (db-table self)) 
 ;;            (adjoin item (gethash (slot-value item slot) (db-table self)))))
;;   (table-index-fields item))
  (values))

(defun index-insert-item (table item)
  (mapc
   #'(lambda (slot)
       (table-intern table (slot-value item slot) item)
       (table-intern table slot item))
   (table-index-fields item)))


;; removes items from db-table and db-all
(defmethod remove-item ((self db) pred)
  (maphash
   #'(lambda (key value)
       (cond ((some pred value)
              (setf (gethash key (db-table self)) (filter value pred)))))
   (db-table self))
  (setf (db-all self)
        (filter (db-all self) pred)))

(defmethod sort-db ((self db))
  (setf (db-all self)
        (msort (db-all self)))
  (do ((count 0 (+ 1 count))
       (lst (db-all self) (cdr lst)))
      ((null lst))
    (setf (slot-value (car lst) 'index) count)))

(defmethod no-pp-fields ((self db))
  (list 'all))

(defmethod no-print-readable-fields ((self db))
  (list 'table 'synonym-table 'current-items 
        'all 'update-list 'struct))


;; OK 2/22/93
(defun check-range (num limit)  
  (cond ((or (minusp num)
             (>= num limit))
         (let ((new-num (error "number out of range: ~a.  [max = ~a]" num limit)))
           (check-range new-num limit)))
        (else num)))


(defun find-root-db (db)  
  (if (db-parent db)
    (find-root-db (db-parent db))
    db))
