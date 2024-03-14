;; bits and bytes


;; bits
;; =========

(setf boole-list
      '(boole-clr
        boole-set
        boole-1
        boole-2
        boole-c1
        boole-c2
        boole-and
        boole-ior
        boole-xor
        boole-eqv
        boole-nand
        boole-nor
        boole-andc1
        boole-andc2
        boole-orc1
        boole-orc2))

(defun doit (b)
  (format t "~%(boole ~(~S~) 3 5): ~B"
          b
          (boole (symbol-value b) 3 5)))

; logior, logxor, logand, logeqv take &rest args
; lognand, lognor, logandc1, logandc2, logorc1, logorc2


; lognot

; logtest

; logbitp

; ash

; logcount

; integer-length

;; bytes
;; =============

; (byte size position)

; byte-size

; byte-position

; ldb 

; ldb-test
; mask-field
; dpb
; deposit-field


; bit array functions page 453
;;============================

; bit, sbit
; bit-and, bit-ior, ...

