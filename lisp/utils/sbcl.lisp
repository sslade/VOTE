;; implementation specific functions
;; Allegro versions



;; 11/24/2015
(defun hpos (stream)
;  (stream:stream-line-column stream))
  (sb-kernel:charpos stream))


;; 11/24/2015
(defun line-length (stream)
  (sb-impl::line-length stream))



;; MCL has a copy-instance.  Allegro does not.
(defun copy-instance (object)
  (let* ((class (class-of object))
         (slots (all-the-slots object))
         (newobj (make-instance class)))
    (mapc #'(lambda (slot)
              (setf (slot-value newobj slot) 
                    (slot-value object slot)))
          slots)
    newobj))


;; MCL has function-name.  Allegro does not.

(defun function-name (f)
  (check-type f function)
  (let* ((str (format nil "~A" f))
         (rstr (reverse str))
         (end (position #\space rstr)))
    (read-from-string (reverse (subseq rstr 1 end)))))


;; The following switch turns off package lock error checking.
;; This is needed because Allegro complains that we have a new
;; type "MEMBER" when it already has a function "MEMBER"
;; This is really bogus.  Allegro should be able to tell the
;; difference between a type and a function.  Sigh.

(sb-ext:unlock-package (find-package 'common-lisp-user))

;; create default method for print-object
(defmethod print-object (obj (stream stream))
  (pprint obj stream))

