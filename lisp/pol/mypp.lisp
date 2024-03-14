
;; hack to get around pp vagaries

; this broke stuff when reading forms like #{Stance ...} 
;(defmethod my-pretty-print ((obj list) port)  
; (let ((pp-obj (read-string->list (format nil "~A" obj))))
;   (pretty-print pp-obj port))


(defmethod my-pretty-print (obj stream)
  (pretty-print obj stream))



