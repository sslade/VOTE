
;;----------------------------------------------------------
;;      Move
;;----------------------------------------------------------

;; move current item to db specified in args
;; or default to parent db


(defun move_command (&rest args)
  (setf args (car args))
  (let ((new_db (or (gethash args *db-table*)
                    (gethash (spell-match args *db-table*) *db-table*)
                    (db-parent *current-db*)))                   
        (items (db-current-items *current-db*)))
    (mapc #'(lambda (item)
            (cond ((not (db? new_db))
                   (format t "~%Must specify DB name as argument to MOVE.~%"))
                  (else
                   (push-update item 'move_command new_db *current-db*)
                   (move-item item *current-db* new_db))))
          items)))


(defun move-item (item old_db new_db)  
  (insert-item new_db item)
  (remove-item old_db #'(lambda (x) (eq x item)))
  (let ((item-db (db item))) 
    (if (db? item-db)
        (set_new_db_prompt item-db new_db)))
  (sort-db new_db)
  (sort-db old_db)
  item)


(defun move_undo (&rest args)  
  (let ((new_db (or (gethash args *db-table*)
                    (db-parent *current-db*)))
        (items (db-current-items *current-db*))
        (old_db *current-db*))
    #'(lambda ()
      (mapc #'(lambda (item)
              (pop-update *current-db*)
              (move-item item new_db old_db))
            items))))


;;----------------------------------------------------------
;;      Pending
;;----------------------------------------------------------

(defun pending_command (&rest args)  
  (declare (ignore args))
  (set_status_command "Pending"))

;; NOTE: undone with status_undo



;;----------------------------------------------------------
;;      Print
;;----------------------------------------------------------



(defun print_command (&rest args)  
  (let ((print-fn (case (car args)
                    ((short headers header) #'print-header)
                    (otherwise #'print-hc)))
        (db (find-root-db *current-db*)))
    (format t "Printing data base to file: ~A~%" (db-print-file *current-db*))
    (with-open-stream 
        (stream (open (db-print-file *current-db*) 
                      :direction :output :if-exists :supersede))
      (time-stamp stream)
      (mapc 
       #'(lambda (item)
         (funcall print-fn item stream args))
       (db-all *current-db*)))
    (setf (db-print-date db) (current_date))
    (push-update #'db-print-date 'print_command db)))

;;----------------------------------------------------------
;;      Print_Date
;;----------------------------------------------------------
;;   Print date that database was last printed.

(defun print_date_command (&rest args)  
  (declare (ignore args))
  (let ((date (db-print-date (find-root-db *current-db*))))
    (cond ((date? date)
           (format t "Last printed: ")
           (pretty-print date *standard-output*))
          (else
           (format t "No print date recorded.~%")))))

;;----------------------------------------------------------
;;      Quit
;;----------------------------------------------------------

(defun quit_command (&rest args)  
  (declare (ignore args))
  'quit)


;;----------------------------------------------------------
;;      Save
;;----------------------------------------------------------


(defun save_command (&rest args)  
  (declare (ignore args))
  (let ((root-db (find-root-db *current-db*)))
    (let* ((datafile (db-data-file root-db))
           (datafile-create-date (file-create-date datafile)))
      (format t "Saving data base updates in ~A~%" datafile)
      (with-open-stream
          (stream (open datafile :direction :output :if-exists :supersede))
        (format stream ";; ~A~%" datafile)
        (save_alias_list stream root-db)
        (save_completed_items 
         datafile
         (save_child_db root-db stream)))
      (reset-create-date datafile datafile-create-date)
      (values))))
                   
(defun save_child_db (db stream)  
    (let ((completed-items nil))
       (cond ((db-parent db) 
              (format stream "~%~%;; Database: ~A~%" (id db))
              (print-readable db stream)
              (format stream "~%(SETF (db-class ~A) (db-class ~A))" 
                           (id db) (id (db-parent db)))
              (format stream "~%(init-db ~A)" (id db))))
       (format stream "~%~%;; Items for Database: ~A" (id db))
       (mapc 
         #'(lambda (item)
            (if (db? (item-db item))
                (setf completed-items
                    (append completed-items
                            (save_child_db (item-db item) stream))))
            (cond ((deleted? item)
                   (print-header item *standard-output*))
                  ((closed? item)
                   (if (null (date-closed item))
                       (setf (date-closed item) (current_date)))
                   (push completed-items item))
                  (else
                   (print-readable-item item stream db))))
             (db-all db))
        completed-items))


(defun print-readable-item (item stream db)  
  (print-readable item stream)
  (format stream "~%(insert-item ~A ~A)"
	  (id db) (id item)))

;; save completed items in separate file
(defun save_completed_items (datafile items)  
 (if items
   (let ((completed-file (concatenate 'string datafile ".comp")))
      (format t "~&Appending completed items to ~A~%" completed-file)
      (with-open-stream
        (stream (open completed-file :direction :output
                      :if-exists :append))
        (mapc 
          #'(lambda (item)
            (print-header item *standard-output*)
            (print-readable-item item stream *current-db*))
          items))
    (values))))


(defun save_alias_list (stream db)  
  (cond ((null (db-aliases db))
         nil)
        (else
         (format stream "~%;; list of command aliases.")
         (format stream "~%(SETF (db-aliases ~A) " (db-name db))
         (print-readable (db-aliases db) stream)
         (format stream ")~%"))))


;;----------------------------------------------------------
;;      Save updates
;;----------------------------------------------------------

(defun save_updates_command (&rest args)  
  (declare (ignore args))
  (let ((updatefile (update-file (db-data-file (find-root-db *current-db*))))
        (updatelist (db-update-list (find-root-db *current-db*))))
    (with-open-stream
        (stream (open updatefile :direction :output
                       :if-exists :append :if-does-not-exist :create))
      (mapc 
       #'(lambda (item+db)
         (if item+db
             (save_update_item item+db stream)))
       updatelist))
    (clear-update-list *current-db*)))

(defun update-file (path)
  (append-pathname path "upd"))
 ;; (pathname (format nil "~A.upd" (namestring path))))
 ;; (merge-pathnames path ".upd")

(defun clear-update-list (db)  
      (setf (db-update-list (find-root-db db)) nil))

(defun save_update_item (item+db stream)  
  (let ((item (car item+db))
        (db (cadr item+db))
        (olddb (caddr item+db)))
    (cond ((functionp item)
           (format stream "~%(SETF (~A " (function-name item))
           (print-readable2 db stream)
           (format stream ") ")
           (print-readable (funcall item db) stream)
           (format stream ")~%"))
          (else
           (format stream "~%(remove-item ~A #'(lambda (i) (eq (id i) '~A)))"
                   (id (or olddb db)) (id item))
           (print-readable-item item stream db)))))


;;----------------------------------------------------------
;;      Show Updates
;;----------------------------------------------------------

(defun show_updates_command (&rest args)  
  (declare (ignore args))
  (let* ((root-db (find-root-db *current-db*))
         (item-list (db-update-list root-db)))
    (cond (item-list
           (set-current-items *current-db* (mapcar #'car (filter item-list #'null)))
           (headers_command '(current))
           nil)
          (else nil))))

(defun show_updates_undo (&rest args)  
  (declare (ignore args))
  (let ((old-current-items (db-current-items *current-db*)))
    #'(lambda ()
      (set-current-items *current-db* old-current-items)
      )))

;;----------------------------------------------------------
;;      Sort
;;----------------------------------------------------------
;;;
;;; redefined in  /lisp/users/slade/a/pol/c/t/sort.t

; (defun sort_command (&rest args)  
;  (declare (ignore args))
;  (sort-db *current-db*))

(defun sort_undo (&rest args)  
  (declare (ignore args))
  (let ((old-db (db-all *current-db*)))
    #'(lambda () (setf (db-all *current-db*) old-db))))

;;----------------------------------------------------------
;;      Synonyms
;;----------------------------------------------------------

(defun synonym_command (&rest args)  
  (let ((db (find-root-db *current-db*)))
    (cond ((null (car args))
           (format t "~%Synonyms:")
           (mapc #'(lambda (synonym)
                     (format t "~%~a~20t" (car synonym))
                     (print-list *standard-output* (cdr synonym)))
                 (db-synonym-alist db))
           (values))
          (else
           (intern-synonyms args)
           (push args (db-synonym-alist db))
           (push-update #'db-synonym-alist 'synonym_command db)))))

(defun intern-synonyms (pair)  
  (let ((table (db-synonym-table (find-root-db *current-db*))))
    (setf (gethash (car pair) table) (cadr pair))
    (setf (gethash (cadr pair) table) (car pair))))

(defun unintern-synonyms (pair)  
  (let ((table (db-synonym-table (find-root-db *current-db*))))
    (table-unintern table (car pair) (cadr pair))
    (table-unintern table (cadr pair) (car pair))))

(defun synonym_undo (&rest args)  
  (declare (ignore args))
  #'(lambda () 
    (let* ((db  (find-root-db *current-db*))
           (pair (pop (db-synonym-alist db))))
      (cond (pair
             (unintern-synonyms pair)
             (format t "~%Removing synonyms: ~A~%" pair)
             (pop-update db))             
            (else 
             (format t "~%Synonym list is empty.~%")))
      (VALUES))))


;;----------------------------------------------------------
;;      Undo
;;----------------------------------------------------------


;; Note: (pop *undo-list*) gets done twice to remove the UNDO command itself.
(defun undo_command (&rest args)  
  (declare (ignore args))
  (pop *undo-list*)
;;    (format t "~%*undo-list* : ~A~%" *undo-list*)
  (cond ((null *undo-list*)
         (format t "~%Nothing more to Undo.~%"))
        (else
         (let ((cmd (pop *undo-list*)))
           (funcall cmd)))))


;;  see process_command above
(defun process_undo_command (command args)  
  (let ((undo-proc (command-undo command)))
    (cond (undo-proc
           (push (apply undo-proc args) *undo-list*))
          (else
           (push #'(lambda () 
                     (format t "~%I don't know how to undo ~A~%"
                             (cons (command-id command) args)))
                 *undo-list*)))))


;;----------------------------------------------------------
;;      Master List of Commands 
;;----------------------------------------------------------

(defstruct command 
    (id        nil)        ;   main name
    (names     nil)        ;   list of command abbreviations
    (procedure nil)        ;   procedure to be invoked   
    (help      nil)        ;   string printed by help procedure
    (undo      nil))       ;   undo procedure for command

(defmethod print-object ((self command) stream)
  (format stream "#<command ~A (~A)>"
          (command-id self)
          (object-hash self)))


;;----------------------------------------------------------
;;      Command Utilities
;;----------------------------------------------------------


(defun init-command-list ()
  (setf *command-list* 
   (append
    (mapcar
       #'(lambda (val-list)
          (let ((cmd (make-command)))
            (setf (command-id cmd) (pop val-list))
            (setf (command-names cmd) (pop val-list))
            (setf (command-procedure cmd) (pop val-list))
            (setf (command-undo cmd) (pop val-list))
            (setf (command-help cmd) (pop val-list))
            cmd))
       `((help (help) ,#'help_command () "print help information")
         (quick-help (?) ,#'help_command_short () "print command names")
         (active (active undelete undel) ,#'active_command ,#'status_undo 
                 "change status to active")
         (add  (add a) ,#'add_command ,#'add_undo "add an item")
         (alias (alias) ,#'alias_command ,#'alias_undo "create a command synonym")
         (and  (and)  ,#'and_command () "selection conjunction")
         (append (append app) ,#'append_command ,#'append_undo "append to field")
 ;;;;        (area-code (area-code ac) ,#'area_code_command () "print area code data")
         (compile (compile) ,#'compile_command () "compile data file")        
         (compile-date (compile-date) ,#'compile_date_command () 
                       "date of last compilation")        
         (completed  (completed complete)  ,#'completed_command ,#'status_undo 
                     "flag current item as completed")               
         (copy (copy cp) ,#'copy_command ,#'copy_undo "copy an entry")
         (delete  (delete del)  ,#'delete_command ,#'status_undo "delete current item")
         (display  (display dis d)  ,#'display_command () "display current item")
         (edit (edit ed e) ,#'edit_command ,#'edit_undo "edit a field")
         (exit (exit x) ,#'exit_command () "save current updates and exit")
         (expunge (expunge exp) ,#'expunge_command () "remove deleted items")
         (get (get g) ,#'get_command ,#'get_undo "get old db")
         (headers (headers header head h ls) ,#'headers_command () "print item headers")
         (lock  (lock)  ,#'lock_command ,#'status_undo "flag current item as locked")
         (pending (pending pend p) ,#'pending_command ,#'status_undo 
                  "change status to pending")
         (print (print) ,#'print_command  () "print current db to file")
         (print-date (print-date) ,#'print_date_command () "date of last printing")  
         (quit (quit bye q) ,#'quit_command  () "exit without saving current updates")
         (save (save s) ,#'save_updates_command  () "save current updates")
         (show-updates (show-updates updates) ,#'show_updates_command 
                       ,#'show_updates_undo "show updated items")
         (sort (sort) ,#'sort_command ,#'sort_undo "sort items")
         (syn (syn index) ,#'synonym_command ,#'synonym_undo "create an index synonym")

         (cdb (cdb cd) ,#'change_db_command ,#'change_db_undo 
              "change to parent/child db")
         (mkdb (mkdb) ,#'make_db_command ,#'make_db_undo "make child db at current item")

         (move (move mv) ,#'move_command ,#'move_undo "move current item to given db")
         (undo (undo u) ,#'undo_command  () "undo last command")))

    (mapcar 
       #'(lambda (val-list)
           (let ((cmd (make-command))
                 (id    (car val-list))
                 (names (cadr val-list)))
             (setf (command-id cmd) id)
             (setf (command-names cmd) names)
             (setf (command-procedure cmd) 
                   #'(lambda (&rest args) (update-field id args)))
             (setf (command-help cmd) 
                   (format nil "update the ~a field" 
                           (capitalize id)))
             (setf (command-undo cmd)
                   #'(lambda (&rest args) (undo-field id args)))
             cmd))
       (db-commands *current-db*)
          )))
  (setf *command-alist* (make-command-alist *command-list*)) )



(defun make-command-alist (cmd-list)  
  (if (null cmd-list)
      nil
      (append (make-alist (command-names (car cmd-list)) (car cmd-list))
              (make-command-alist (cdr cmd-list)))))

(defun make-alist (key-list item)  
  (cond ((null key-list) nil)
        (else (cons (cons (car key-list) item)
                    (make-alist (cdr key-list) item)))))


(defun update-field (slot args)  
    (let* ((proc (update-prototype (car (db-current-items *current-db*)) slot))
           (val (cond ((car args))
                     (else (funcall proc)))))
;;           (read-line (standard-input))    ;; <<-- clear buffer...
        (mapc #'(lambda (item)
                  (push-update item slot *current-db*)
                  (setf (slot-value item slot) val)
                  ;; index update, if appropriate
                  (if (member slot (table-index-fields item))
                    (setf (gethash (slot-value item slot) (db-table *current-db*))
                          item))
                  (print-header item *standard-output*))
              (db-current-items *current-db*))))
      


(defun undo-field (slot args)  
  (declare (ignore args))
  (let* ((items (db-current-items *current-db*))
         (old-values (mapcar #'(lambda (item) (slot-value item slot)) items)))
    #'(lambda () 
        (mapc 
         #'(lambda (item value)
             (pop-update *current-db*)
             (format t "~%Resetting ~A slot of ~A to ~A~%" slot (id item) value)
             (setf (slot-value item slot) value))
         items old-values))))



;;----------------------------------------------------------
;;      End of file: db_cmds2.t
;;----------------------------------------------------------

