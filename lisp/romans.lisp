(defun romans (limit n)
  "Generate all N-digit roman numbers less than or equal to limit."
  (romans2 limit n 1 nil))

(defun romans2 (limit n count result)
  (if (> count limit)
    result
    (let ((numeral (format nil "~@r" count)))
      (if (= (length numeral) n)
        (push numeral result))
      (romans2 limit n (1+ count) result))))