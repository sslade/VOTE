
;; language modules

(defvar *gen-files*)
(setf *gen-files*
      '("specials_gen"
        "clusters"
        "context" 
        "english" 
        "gen_utils" 
        "bill-english"
        "nouns"
        "np" 
        "generics"
        "preamble" 
        "rels"
        "verbs" 
;;        "combine"  ;; *siblings* issue
        "downside" 
        "vp" 
        "test"
        "explain"  ;; define-predicate issue
	))

(my-load-files gen *gen-files*)
