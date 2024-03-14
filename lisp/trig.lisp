; 1, 2, 3

(sin pi)
(cos pi)
(tan pi)

(defun sin-degree (x)
  (sin (* x (/ (float pi x) 180))))

(defvar 2pi (* 2 pi))
(defvar pi/2 (/ pi 2))
(defvar pi/4 (/ pi 4))

(defun trig-test (fn)
  (let ((func (symbol-function fn)))
    (format t "~A~t0~tpi/4~tpi/2~tpi~t2pi~%" fn)
    (format t "~t~A~T~A~T~A~T~A~T~A~%"
            (funcall func 0) 
            (funcall func pi/4)
            (funcall func pi/2)
            (funcall func pi)
            (funcall func 2pi))))




; secant
(defun secant (n) (/ 1 (cos n)))

(defun cosecant (n) (/ 1 (sin n)))

(defun cotangent (n) (/ 1 (tan n)))

; inverse functions (UVBA page 82)
(defun isin (x) (atan (/ x (sqrt (+ (* (- x) x) 1)))))

; 4, 5, 6
; arc functions
(defun arc-sin (x) 
  (let ((i (complex 0 1)))
    (* (- i) 
       (log (+ (* i x) 
               (sqrt (- 1 (* x x))))))))

(defun arc-cosine (x) 
  (let ((i (complex 0 1)))
    (* (- i) 
       (log (+ x
               (* i (sqrt (- 1 (* x x)))))))))


; 7, 8, 9
; Hyperbolic 
; sinh
(defun hyp-sine (x) (/ (- (exp x) (exp (- x))) 2))

; cosh
(defun hyp-cosine (x) (/ (+ (exp x) (exp (- x))) 2))

; tanh
(defun hyp-tangent (x) (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))

; 10, 11, 12
; asinh

; acosh

; atanh

