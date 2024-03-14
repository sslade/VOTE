
(defun say-bill-p/c (billid stream)  
  (let* ((i (if (bill-p billid)
                billid
                (get-node billid bill)))
         (pro (bill-stance-for i))
         (con (bill-stance-agn i)))
    (init-context)
    (if pro
        (say-bill-side i 'pro pro stream))
    (if con
        (say-bill-side i 'con con stream))
    (values)))


(defun say-bill-side (bill side reasons stream)  
  (let ((phrase (case side
                  ((pro) "support of ")
                  ((con) "opposition to "))))
    (say-cluster-reasons (proto-dnp bill-np
                            'bill t
                            'use-noun t
                            'lex (concatenate 'string phrase (gen-np bill 'obj)))
                         reasons
                         stream)))


(defun fix-latex-dollar (str)  
  (cond ((string= "" str) str)
        ((eq #\$ (char str 0))
         (concatenate 'string 
          "\\$"
          (fix-latex-dollar (subseq str 1))))
        (else
         (concatenate 'string 
          (->string (char str 0))
          (fix-latex-dollar (subseq str 1))))))
