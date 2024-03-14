(in-package common-lisp-user)

;;;;    -------------------------------------------------------------
;;;;
;;;;    Table of Contents:
;;;;    
;;;;    (my-load directory filename)
;;;;    
;;;;    (recompile-if-source-is-newer source-file object-file)
;;;;    
;;;;    (my-compile-file directory filename)
;;;;    
;;;;    -------------------------------------------------------------


;; Handles default file extensions and automatic compilation:

;; If a file extension is given, load the given file;
;; else, if compiled (fasl) version exists, load it,
;;       but first recompile file if source is newer
;; else, load source (cl) version.
;; -----------------------------------------------------------

(defun my-load (directory filename)
  (let* ((file-pathname (merge-pathnames filename directory))
         (source-file
          (merge-pathnames (make-pathname :type "lisp") file-pathname))
         (object-file
          (merge-pathnames (make-pathname :type *compiled-extension*) file-pathname)))
    
    (cond ((pathname-type file-pathname)
	   ;;; added 10/6/2018
	   ;;;(compile-file file-pathname)
           (load file-pathname))
          ((probe-file object-file)
           (recompile-if-source-is-newer source-file object-file)
           (load object-file))
          ((probe-file source-file)
           (load source-file))
          (else
           (error "; No object or source version of file ~s~%" filename)))))
 
;; If both source and object versions exist and the source version
;; has a more recent write date, then recompile the source file.
;; -----------------------------------------------------------

(defun recompile-if-source-is-newer (source-file object-file)
  (cond ((and (probe-file object-file)
              (probe-file source-file)
              (> (file-write-date source-file)
                 (file-write-date object-file)))
         (format t "~%; ~s: Source is newer than object.  Recompiling.~%" 
                 (pathname-name source-file))
        (let ((object-file-create-date (file-write-date object-file)))
           (compile-file source-file)
           (reset-create-date object-file object-file-create-date)))))



;; Allows compilation using directory pathnames
;; -----------------------------------------------------------

(defun my-compile-file (directory filename)
  (let ((file-pathname (merge-pathnames filename directory)))
    (compile-file file-pathname)))


;; The file synchronization utility on the mac uses the creation
;; date to determine if two files are EQ.
;; Thus, we want to maintain creation date identity for purposes of synchronization.
;;----------------------------------------------------------------------------------

(defun reset-create-date (file date)
  (format t "~%; File: ~A ~%; Create Date: ~A (~A)~%" file (universal-date-to-string date) date)
 ;;  (if date (set-file-create-date file date)))
  )


(defun universal-date-to-string (d)
  (multiple-value-bind
    (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
    (decode-universal-time d)
    (declare (ignore day-of-week daylight-saving-time-p time-zone))
    (format nil "~A/~A/~A ~A:~A:~A" month date year hour minute second)))


    
