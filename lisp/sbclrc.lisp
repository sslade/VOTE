(in-package common-lisp-user)

(declaim (sb-ext:muffle-conditions style-warning))

; (setf *load-verbose* t)
; (setf *load-print* t)

(defvar else t)
(defvar *compiled-extension* "fasl")
(sb-ext:unlock-package (find-package 'common-lisp-user))
(sb-ext:unlock-package (find-package 'common-lisp))

(format t "~%~%Welcome to Slade's LISP!")



