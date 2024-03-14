
;; Globals
;;    *current-db*
;;    *undo-list*
;;    *db-table*
;;    *command-list*
;;    *split-file-size*

;; Active
;;    (active_command . args)

;; Add
;;    (add_command . args)
;;    (input-values item)
;;    (add_undo . args)

;; Alias
;;    (alias_command . args)
;;    (alias_undo . args)

;; And
;;    (and_command . args)
;;    (intersect* . args)
;;    (keyword-select input)
;;    (keyword-select2 input)

;; Append
;;    (append_command . args)
;;    (append-field slot args)
;;    (append_undo . args)

;; Area codes (??!!)
;;    (area_code_command . args)

;; Changing and Making db's
;;    (change_db_command . args)
;;    (change_db_undo . args)
;;    (make_db_command . args)
;;    (set_new_db_prompt new-db)
;;    (make_db_undo . args)

;; Compile
;;    (compile_command . args)
;;    (split_file datafile size)

;; Compile_Date
;;    (compile_date_command . args)

;; Completed / Closed
;;    (completed_command . args)

;; Delete
;;    (delete_command . args)
;;    (set_status_command status)
;;    (status_undo . args)

;; Display
;;    (display_command . args)

;; Exit
;;    (exit_command . args)

;; Expunge
;;    (expunge_command . args)

;; Get
;;    (get_command . args)
;;    (db-tree-sort db)
;;    (get_undo . args)

;; Headers
;;    (headers_command . args)

;; Help
;;    (help_command . args)
;;    (help_command_short . args)

;; Lock
;;    (lock_command . args)

;; Move
;;    (move_command . args)
;;    (move-item item old_db new_db)
;;    (move_undo . args)

;; Pending
;;    (pending_command . args)

;; Print
;;    (print_command . args)

;; Print_Date
;;    (print_date_command . args)

;; Quit
;;    (quit_command . args)

;; Save
;;    (find-root-db db)
;;    (save_command . args)
;;    (save_child_db db stream)
;;    (print-readable-item item stream db)
;;    (save_completed_items datafile items)
;;    (save_alias_list stream db)

;; Save updates
;;    (save_updates_command . args)
;;    (clear-update-list db)
;;    (save_update_item item+db stream)

;; Show updates
;;    (show_updates_command . args)
;;    (show_updates_undo . args)

;; Sort
;;    (sort_command . args)
;;    (sort_undo . args)

;; Synonyms
;;     (synonym_command . args)
;;     (intern-synonyms pair)
;;     (unintern-synonyms pair)
;;     (synonym_undo . args)

;; Undo
;;    (undo_command . args)
;;    (process_undo_command command args)

;; Command structure type, with method:
;;    (print self stream)

;; Command Utilities
;;    (init-command-list)  [which sets *command-list*]
;;    (make-command-alist cmd-list)
;;    (make-alist key-list item)
;;    (update-field slot args)
;;    (undo-field slot args)


;;----------------------------------------------------------
;;      Other command files...
;;----------------------------------------------------------

;(require "edit")

;;----------------------------------------------------------
;;      Active
;;----------------------------------------------------------

(defun active_command (&rest args)  
  (declare (ignore args))
  (set_status_command "Active"))

;; NOTE: undone with status_undo



;;----------------------------------------------------------
;;      Add
;;----------------------------------------------------------

(defun add_command (&rest args)  
  (declare (ignore args))
  ;;    (format t "Adding new item...~%")
;;  (let ((item ((stype-constructor (db-class *current-db*))))) 
  (let ((item (make-instance (db-class *current-db*))))
    (input-values item)
    (insert-item *current-db* item)
    (push-update item 'add_command *current-db*)
    (sort-db *current-db*)))

(defun input-values (item)  
;;  (read-line (standard-input))    ;; <<-- clear buffer...
    (mapc
      #'(lambda (slot)
          (let ((proc (input-prototype item slot)))
            (setf (slot-value item slot) (funcall proc))))
      (all-the-slots item))
    item)

(defun add_undo (&rest args)  
  (declare (ignore args))
  #'(lambda ()
    (pop-update *current-db*)
    (format t "~%Removing update entry.~%")))
 
 

;;----------------------------------------------------------
;;      Alias
;;----------------------------------------------------------

(defun alias_command (&rest args)  
  (let ((db (find-root-db *current-db*)))
    (cond ((null (car args))
           (format t "~%aliases:")
           (mapc #'(lambda (alias)
                   (format t "~%~a~20t" (car alias))
                   (print-list *standard-output* (cdr alias)))
                 (db-aliases db))
           (values))
          (else
           (push args (db-aliases db))
           (push-update #'db-aliases 'alias_command db)))))

(defun alias_undo (&rest args)  
  (if args
    #'(lambda () "Nothing to undo.~%")
    (let* ((db (find-root-db *current-db*))
           (cmd (car (db-aliases db))))
      #'(lambda () 
          (cond (cmd
                 (pop (db-aliases db))
                 (pop-update db)
                 (format t "~%Removing alias command: ~A~%" cmd))
                (else 
                 (format t "~%Alias list is empty.~%")))
          (values)))))

;;----------------------------------------------------------
;;      And command
;;----------------------------------------------------------

(defun and_command (&rest args)  
  (let ((selection
         (apply #'intersect* (mapcar #'keyword-select args))))
    (set-current-items *current-db* selection)
    (headers_command '(current))))


(defun intersect* (&rest args)  
  (cond ((null (cdr args))
         (car args))
        (else
         (intersection (car args)
                       (apply #'intersect* (cdr args))))))

(defun keyword-select (input)  
  (let* ((hits (keyword-select2 input))
         (hit-count (length hits)))
    (cond (hits
           (format t "~A Hit~P.~%"
                   hit-count hit-count))
          (else (format t "No hits for ~A.~%" input)))
    hits))
           

(defun keyword-select2 (input)  
;;  check for index to db item using spelling correction then synonyms
  (let* ((table (db-table *current-db*))
         (synonyms (gethash input (db-synonym-table (find-root-db *current-db*))))
         (word (spell-match table input)))
    (cond (word
           (if (eq input word)
             (format t "~%Keyword index: ~A~%" word)
             (format t "~%Keyword index with spelling correction: ~A ==> ~A~%"
                     input
                     word))
           (gethash word table))
          (synonyms
           (let ((words (collect synonyms #'(lambda (w) (gethash w table)))))
             (cond (words
                    (format t "~%Keyword index with synonym~P: ~A ==> ~A~%"
                            (length synonyms)
                            input
                            synonyms)
                    (collect words #'(lambda (w) (gethash w table))))
                   (else nil))))
          (else
           nil))))
     

;;----------------------------------------------------------
;;      Append
;;----------------------------------------------------------

(defun append_command (&rest args)  
  (let ((slot (car args)))
    (append-field slot (cdr args))))

(defun append-field (slot args)  
    (let* ((items (db-current-items *current-db*))
           (proc (update-prototype (car items) slot))
           (val (cond ((car args))
                      (else (funcall proc)))))
;;           (read-line (standard-input))    ;; <<-- clear buffer...
      (mapc #'(lambda (item)
              (let ((old_val (slot-value item slot)))
                (push-update item slot *current-db*)
                (setf (slot-value item slot)
                     (cond ((or (consp old_val)
                                (null old_val))
                            (append old_val val))
                           ((stringp old_val)
                            (concatenate 'string old_val val))
                           (else old_val)))
                ;; index update, if appropriate
                (if (member slot (table-index-fields item))
                    (setf (gethash (slot-value item slot) (symbol-value (db-table *current-db*)))
                          item))
                (print-header item *standard-output*)))
            items)))
      

(defun append_undo   (&rest args)
  #'(lambda ()
      (undo-field (caar args) (cdr args))))


;;----------------------------------------------------------
;;     Area codes
;;----------------------------------------------------------

;(defun area_code_command (&rest args)  
;  (let ((ac (car args)))
;    (if (numberp ac) (setf ac (concatenate-symbol 'ac ac)))
;    (cond ((area-code? (ac-table ac))
;           (pp (ac-table ac)))
;          ((and (ac-table ac) 
;                (listp (ac-table ac)))
;           (mapc #'(lambda (areac)
;                   (pretty-print areac *standard-output*)
;                   (format t "~%~%"))
;                 (ac-table ac)))
;          (else
;           (format t "~&Not a valid area-code: ~A.~&" ac)
;           nil))))

;;----------------------------------------------------------
;;      Changing and Making data bases
;;----------------------------------------------------------
;;
;;  change to different type of db 
;;
;;  move up or down the db hierarchy

(defun change_db_command (&rest args)  
  (setf args (car args))
  (let* ((child (car (db-current-items *current-db*)))
         (new-db
          (cond (args
                 (or (and (db? args)
                          args)
                     (and (db? (gethash args *db-table*))
                          (gethash args *db-table*))
                     (gethash (spell-match *db-table* args) *db-table*)
                     (db-parent *current-db*)
                     *current-db*))
                ((not (db? (slot-value child 'db)))
                 (format t "~%Current item not a data base.~%")
                 nil)
                (else
                 (slot-value child 'db))))
         (old-db *current-db*))
    (cond ((db? new-db)
           (setf *current-db* new-db)
           (cond ((eq (db-class new-db) (db-class old-db))
                  nil)
                 (else
                  ;; now have a general item? definition
                  ;; (defun item? (obj) (typep obj (db-class new-db)))
                  (init-command-list)
                  (values))))
          (else nil))))


(defun change_db_undo (&rest args)  
  (declare (ignore args))
  (let ((old-db *current-db*))
    #'(lambda ()
      (change_db_command old-db))))

(defun make_db_command (&rest args)  
  (declare (ignore args))
  (let ((new-db (copy-instance *current-db*)))
    (setf (db-name new-db) (car (prompted-list-input "DB Name (1 word)? ")))
    (set_new_db_prompt new-db *current-db*)
    (setf (slot-value (car (db-current-items *current-db*)) 'db) new-db)
    (init-db new-db)))

(defun set_new_db_prompt (new-db parent-db)  
  (setf (db-parent new-db) parent-db)
  (setf (db-prompt new-db) 
       (concatenate 'string 
        (if parent-db
            (concatenate 'string (string (db-name parent-db)) ".")
            "")
        (string (db-name new-db)) "> ")))

(defun make_db_undo (&rest args)  
    (undo-field 'db args))

;;  have a db-table for each db?  automatically updated from subject line by mkdb?


;;----------------------------------------------------------
;;      Compile 
;;----------------------------------------------------------
;;
;;   Save all data in master file.
;;   Split master file and create loadfile.
;;   Compile the split master files.
;;   Reset master update file.
;;
;;   Number of sexpressions in each data file.
;;   Used to be 200, but compilation seemed to be a nonlinear function
;;    of file size, so I reduced it.
(defconstant *split-file-size* 100)  

(defun compile_command (&rest args)  
  (declare (ignore args))
  (run-time
   (let ((db (find-root-db *current-db*)))
     (cond ((null (db-all *current-db*))
            (format t "~%No data loaded to compile.~%"))
           (else
            (save_command)
            (let* ((datafile (db-data-file db))
                   (updatefile (update-file datafile))
                   (updatefile-create-date (file-create-date updatefile))
                   (outfiles (split_file datafile *split-file-size*)))
              (mapc #'safe-compile-file outfiles)
              (with-open-stream
                  (stream (open updatefile :direction :output :if-exists :supersede
                                :if-does-not-exist :create))
                (format stream ";; update file for ~A~%~%" (id *current-db*)))
              (setf (db-compile-date db) (current_date))
              (push-update #'db-compile-date 'compile_command db)
              (reset-create-date updatefile updatefile-create-date)
              (save_updates_command)
              ))))))

(defun safe-compile-file (source-file)
  (let* ((object-file (format nil "~A.~A" source-file *compiled-extension*))
         (object-file-create-date (file-create-date object-file)))
    (compile-file source-file)
    (reset-create-date object-file object-file-create-date)))

;; splits a file of S-expressions into subfiles of size or fewer S-expressions
;; creates a file called file.load which will load the split files
;; returns a list of those files.


(defun split_file (datafile size)  
  (let* ((loadfile (append-pathname datafile "load"))
         (loadfile-create-date (file-create-date loadfile))
         (loadstream (open loadfile :direction :output :if-exists :supersede 
                           :if-does-not-exist :create))
         (instream (open datafile :direction :input)))
    (labels ((close-streams (outstream)
               (mapc #'close (list instream loadstream outstream))
               nil)
             (copy-part (instream outstream count)
  ;;;             (db-format t "This is COPY-PART: ~A~%" count)
               (if (eof? (peek-char nil instream nil eof))  
                 (close-streams outstream)
                 (let ((input (read instream)))
                   (cond 
                    ((>= count size)
                     ;; (write input :stream outstream)
                     (pprint input outstream)
                     (close outstream)
                     T)
                    (else
                     (write input :stream outstream)
                     (terpri outstream)
                     (copy-part instream outstream (+ count 1)))))))
             (split (instream datafile count)
               (let* ((outfile (format nil "~A~A" datafile count))
                      (outfile-create-date (file-create-date outfile))
                      (outstream (open outfile :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)))
                 (format t "~%Copying from ~A to ~A" datafile outfile)
                 (reset-create-date outfile outfile-create-date)
                 (if (> count 0) (format outstream ";; ~A ~%"
                                         outfile))
                 (format loadstream "(load (merge-pathnames data ~S))~%" 
                         (format nil "~A.~A" (pathname-name outfile) *compiled-extension*))
                 (cond ((copy-part instream outstream 0)
                        (cons outfile (if (open-stream-p instream)
                                        (split instream datafile (+ count 1))
                                        nil)))
                       (else (list outfile))))))
      (format loadstream ";; ~A ~%~%"  loadfile)
      (prog1 (split instream datafile 0)
        (reset-create-date loadfile loadfile-create-date))
      )))

;;----------------------------------------------------------
;;      Compile_Date
;;----------------------------------------------------------
;;   Print date that database was last compiled.

(defun compile_date_command (&rest args)  
  (declare (ignore args))
  (let ((date (db-compile-date (find-root-db *current-db*))))
    (cond ((date? date)
           (format t "Last compiled: ")
           (pretty-print date *standard-output*))
          (else
           (format t "No compile date recorded.~%")))))



;;----------------------------------------------------------
;;      Complete / Closed
;;----------------------------------------------------------

(defun completed_command (&rest args)  
  (declare (ignore args))
  (set_status_command "Completed"))

;; NOTE: undone with status_undo

;;----------------------------------------------------------
;;      Delete 
;;----------------------------------------------------------

(defun delete_command (&rest args)  
  (declare (ignore args))
  (set_status_command "Deleted"))

(defun set_status_command (status)  
  (let ((items (db-current-items *current-db*)))
    (mapc #'(lambda (item)
            (cond ((not (string= (slot-value item 'status) "Locked"))
                   (push-update item 'set_status_command *current-db*)
                   (setf (slot-value item 'status) status))
                  (else
                   (format t "~%Current item locked.  Cannot modify status.~%")))
            ;;      (insert-item *current-db* item)
            (print-header item *standard-output*))
          items)))

(defun status_undo (&rest args)  
  (undo-field 'status args))

;;----------------------------------------------------------
;;      Display
;;----------------------------------------------------------

(defun display_command (&rest args)  
  (declare (ignore args))
  (mapcar #'(lambda (item)
         (pretty-print item *standard-output*))
       (db-current-items *current-db*)))

;;----------------------------------------------------------
;;      Exit
;;----------------------------------------------------------

(defun exit_command (&rest args)  
  (declare (ignore args))
  (save_updates_command)
  (quit_command))


;;----------------------------------------------------------
;;      Expunge
;;----------------------------------------------------------

(defun expunge_command (&rest args)  
  (declare (ignore args))
  (headers_command 'deleted)
  (remove-item *current-db* #'deleted?)
  (sort_command))



;;----------------------------------------------------------
;;      Get
;;----------------------------------------------------------

;; modified to read compiled version of file.

(defun get_command (&rest args)  
  (declare (ignore args))
  (let* ((datafile (db-data-file *current-db*))
         (loadfile (append-pathname datafile "load")))
    (init-db *current-db*)
    (format t "~%; Reading data base from file: ~A~%" datafile)
    (load loadfile)
    ; (load (update-file datafile))
    (clear-update-list *current-db*)
    (mapc #'intern-synonyms (db-synonym-alist *current-db*))
; 11/28/19    (db-tree-sort (find-root-db *current-db*))
; 12/13/19 add the following:
    (sort-db *current-db*)
    ))

(defun db-tree-sort (db)  
  (sort-db db)
  (mapc 
   #'(lambda (item)
       (if (db? (slot-value item 'db))
         (db-tree-sort (slot-value item 'db))))
   (db-all db)))



(defun get_undo (&rest args)  
  (declare (ignore args))
  (let ((db *current-db*))
    #'(lambda () (setf *current-db* db))))



;;----------------------------------------------------------
;;      Headers
;;----------------------------------------------------------

(defun headers_command (&rest args)  
  (let ((selection
         (collect (db-all *current-db*)
                  (get-selection-predicate args))))
    (mapc 
     #'(lambda (item)
       (print-header item *standard-output*))
     selection)))



;;----------------------------------------------------------
;;      Help         
;;----------------------------------------------------------


(defun help_command (&rest args)  
  (labels ((print-command (cmd)
            (print-list *standard-output* (command-names cmd))
            (format t "~26T~A~%" (command-help cmd)))
           (find-command (cmd commands)
            (cond ((null commands)
                   nil)
                  ((memq cmd (command-names (car commands)))
                   (car commands))
                  (else (find-command cmd (cdr commands))))))
    (debug-msg "~%ARGS: ~A~%" args)
    (let ((tmp (and args
                    (find-command (caar args)
                                  *command-list*))))
      (cond (tmp (print-command tmp))
            (else
             (mapc #'print-command *command-list*)
             (if (db-aliases (find-root-db *current-db*))
               (alias_command))))
      (values))))

(defun help_command_short (&rest args)  
  (declare (ignore args))
  (let ((commands (append (mapcar #'(lambda (c) (car (command-names c)))
                               *command-list*)
                          (mapcar #'car (db-aliases (find-root-db *current-db*))))))
    (pp-tab-list *standard-output* (msort commands))))

;;----------------------------------------------------------
;;      Lock
;;----------------------------------------------------------

(defun lock_command (&rest args)  
  (declare (ignore args))
  (set_status_command "Locked"))

;; NOTE: undone with status_undo

;;----------------------------------------------------------
;;      End of file: db_cmds.t
;;----------------------------------------------------------

