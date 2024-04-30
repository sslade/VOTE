;;
;; (japanese self)
;; operation for producing japanese phrase for a structure
;;
;;-------------------------------------------------------
(defmethod japanese (self)
  (cond ((and (record-p self)
              (slot-value self 'name)))
        (else nil)))


;;;;------------------------------------------------------------------------
;;;;    stances
;;;;------------------------------------------------------------------------

;;  State a generic stance


(defun jpn-stance (subject stance)  
  (let ((st (cond ((jpn-flip-stance stance))
                  (else stance))))
    (concatenate 'string (gen-np subject 'subj)
                   (jpn-stance2 subject st)
                   (jpn-stance-issue st)
                   ".")))


(defun jpn-flip-stance (stance)  
  (let ((opp (issue-opposite (reveal-issue stance))))
    (cond ((and (eq (stance-side stance) 'con)
                opp)
           (let ((st1 (copy stance)))
             (setf (stance-side st1) 'pro)
             (setf (stance-issue st1) opp)
             (setf (stance-issue-structure st1) (get-node opp issue))
             st1))
          (else nil))))


(defun jpn-stance2 (subject stance)  
  (let* ((side (stance-side stance)))
    (format nil " ~A "
            (case side
              ((pro) (cond ((is-issue? subject)
                            (japanese-issue-pro subject stance))
                           ((is-record? subject)
                            (japanese-record-pro subject stance))
                           (else 
                            (japanese-pro subject stance))))
              ((con) (cond ((is-issue? subject)
                            (japanese-issue-con subject stance))
                           ((is-record? subject)
                            (japanese-record-con subject stance))
                           (else
                            (japanese-con subject stance))))
              ))))


(setf (symbol-function 'say) (symbol-function 'jpn-stance))

(defmethod is-issue? (self)
  (or (bill-p self)      ;; added for stating bill positions
      (issue-p self)))

;; ?? need to parameterize for PERSON, TENSE, AFFECT

;;;;------------------------------------------------------------------------
;;;;    issue phrases
;;;;------------------------------------------------------------------------
; 
; (define (jpn-stance-issue stance)
;   (japanese-issue (reveal-issue stance) (stance-side stance)))
; 

(defun jpn-stance-issue (stance)  
  (let* ((iss (reveal-issue stance))
         (imp? (<important? 'C (stance-importance stance)))
         (side (stance-side stance))
         (phrase (japanese-issue-phrase iss side))
         (said  (said? *context* iss))
         (short (japanese-short iss))
         (iss-jpn (if imp?
                      (case side
                        ((pro) 
                         (issue-pro-japanese iss))
                        ((con) 
                         (issue-con-japanese iss)))
                      nil)))
    (format nil "~A~A"
            (if (and (not said) phrase)
                (concatenate 'string phrase " ")
                "")
            (cond ((and said short))
                  (said
                   (japanese iss))
                  (iss-jpn)
                  (else (japanese iss)))
            )))
                   

;; the intro phrase is used the first time an issue is mentioned
;; in the current context.

(defun japanese-issue-phrase (iss side)  
  (case (issue-type iss)    
    ((principle) (gen-pick-one
                  nil
                  "信じること"
                  "の原理"))
    ((right)     "の権利")
    ((proposal)  "の提案")
    ((policy)    "の政策")
    ((group)     (case 
                   ((pro) "の正当な懸念")
                   ((con) "の特別な利益")))
    ((cause)     "の原因")
    ((program)   "のプログラム")
    ((nomination) "の指名")
;;    ((reform)    (gen-pick-one
;;                  "efforts to achieve a better"
;;                  "the reform of"))
    ((problem)   "の問題")
    ((foreign)   (gen-pick-one
                  "アメリカの関与"
                  "における我が国の役割"
                  "私たちの国との関係"))
    (otherwise    nil)))             ;; was "the issue of"
    

;;------------------------------------------------------------------------
;;  pro/con issue statements -- human subjects
;;------------------------------------------------------------------------


(defun get-importance (obj)  
  (cond ((relation-p obj) (relation-importance obj))
        ((stance-p obj)   (stance-importance obj))
        (else nil)))

(defun get-side (obj)  
  (cond ((relation-p obj) (relation-side obj))
        ((stance-p obj)   (stance-side obj))
        (else nil)))

(defun japanese-pro (subject stance)  
  (case (get-importance stance)
    ((a) (gen-pick-one
          (format nil "~A 揺るぎない ~A のサポート"
                  (gen-verb subject "は" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A の長年にわたるサポート"
                  (gen-verb subject "応力" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A の根強い支持"
                  (gen-verb subject "強調する" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "揺るぎなく ~A" (gen-verb subject "支持する" nil 'present))
          (format nil "完全に ~A" (gen-verb subject "サポート" nil 'present))
          (format nil "~A のチャンピオン" (gen-verb subject "は" nil 'present))
          (format nil "~A の忠実な擁護者" (gen-verb subject "は" nil 'present))
          (format nil "~A に深くコミットする" (gen-verb subject "は" nil 'present))
          (format nil "~A 常に支持していた" (gen-verb subject "持って" nil 'present))
          (format nil "~A いつも強く感じていたこと" (gen-verb subject "持って" nil 'present))
          (format nil "~A いつも信じていた" (gen-verb subject "持って" nil 'present))))
         
    ((b) (gen-pick-one
          (format nil "強く ~A" (gen-verb subject "スポート" nil 'present))
          (format nil "快く ~A" (gen-verb subject "支持する" nil 'present))
          (format nil "~A の擁護者" (gen-verb subject "は" nil 'present))
          (format nil "~A 強く賛成する" (gen-verb subject "思う" nil 'present))
          (format nil "~A について深く" (gen-verb subject "心配" nil 'present))
          (format nil "~A 熱心にサポートする" (gen-verb subject "は" nil 'present))
          (format nil "~A のために" (gen-verb subject "苦しむ" nil 'present))
          (format nil "~A に専心して" (gen-verb subject "は" nil 'present))
          (format nil "~A 見せたがる ~A へのサポート"
                  (gen-verb subject "は" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A 強く賛成する" (gen-verb subject "は" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A" (gen-verb subject "支持する" nil 'present))
          (format nil "~A に賛成" (gen-verb subject "は" nil 'present))))))


(defun japanese-con (subject stance)  
  (case (get-importance stance)
    ((a) (gen-pick-one
          (format nil "~A 揺るぎない ~A 反対"
                  (gen-verb subject "は" nil 'present)
                  (gen-pronoun subject 'poss))
          (format nil "~A ~A です"
                  (gen-verb subject "は" nil 'present)
                  (if (singular? subject)
                      "長年の対戦相手"
                      "長年の対戦相手"))
          (format nil "~A 断固として反対する" (gen-verb subject "は" nil 'present))
          (format nil "~A 決してサポートされなかった" (gen-verb subject "持って" nil 'present))))
         
    ((b) (gen-pick-one
          (format nil "~A ~A of"
                  (gen-verb subject "は" nil 'present)
                  (if (singular? subject)
                      "対戦相手"
                      "対戦相手"))
          (format nil "~A 対戦相手" (gen-verb subject "は" nil 'present))
          (format nil "~A 強く反対する" (gen-verb subject "は" nil 'present))
          (format nil "強く ~A" (gen-verb subject "反対する" nil 'present))
          (format nil "~A 強く反対する" (gen-verb subject "は" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A とは対照的に" (gen-verb subject "は" nil 'present))
          (format nil "~A" (gen-verb subject "反対する" nil 'present))
          (format nil "~A に" (gen-verb subject "異なる" nil 'present))
          (format nil "~A に対して" (gen-verb subject "は" nil 'present))))))


;;------------------------------------------------------------------------
;;  pro/con statements -- issue subjects
;;------------------------------------------------------------------------

(defun japanese-issue-pro (subject stance)  
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "~A の不可欠な要素" (gen-verb subject "は" nil 'present))
          (format nil "~A 常に一部です" (gen-verb subject "は" nil 'present))
          (format nil "~A にとって根本的に重要な" (gen-verb subject "は" nil 'present))))

    ((b) (gen-pick-one
          (format nil "~A" (gen-verb subject "支持する" nil 'present))
          (format nil "~A" (gen-verb subject "強化する" nil 'present))
          (format nil "~A にとって重要" (gen-verb subject "は" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A と互換性があります" (gen-verb subject "は" nil 'present))
          (format nil "~A と一致する" (gen-verb subject "は" nil 'present))))))


(defun japanese-issue-con (subject stance)
  (case (stance-importance stance)
    ((a) (gen-pick-one
          (format nil "強く ~A" (gen-verb subject "逆らう" nil 'present))
          (format nil "~A 完全に反対" (gen-verb subject "苦しむ" nil 'present))))

    ((b) (gen-pick-one
          (format nil "~A" (gen-verb subject "チャレンジ" nil 'present))
          (format nil "~A" (gen-verb subject "楯つく" nil 'present))))

    ((c d) (gen-pick-one
          (format nil "~A 問題にする" )
          (format nil "~A" (gen-verb subject "問う" nil 'present))
          (format nil "~A に対して" (gen-verb subject "する" nil 'present))
          (format nil "~A 反対に" (gen-verb subject "は" nil 'present))))))


;;;;------------------------------------------------------------------------
;;;;    test code
;;;;------------------------------------------------------------------------
;;
;; Generates an Japanese sentence for a set of issues using either
;; the norm, or a dummy stance.
;;
;; E.g., 
;; (jpn-test 'pro 'b '(aids women abortion))
;;  
;;  ==>
;;    
;;  [()] the average American cares about the issue of AIDS research and education.
;;  [RIGHT] popular opinion approves of the right Women's Rights (*).
;;  [()] the typical American cares about the issue of a woman's right to have an abortion.
;;  
(defun jpn-test (side imp iss-list)  
  (let ((issues (mapcar 
                 #'(lambda (iss)
                   (if (issue-p iss)
                       iss
                       (get-node iss issue)))
                 iss-list)))
    (mapcar 
     #'(lambda (iss)
       (let ((type (if (issue-type iss)
                       (format nil " [~A]" (issue-type iss))
                       "")))
         (cond ((issue-norm iss)
                (format nil "~A"
                        (say (gen-norm-np) (issue-norm iss))
                        ))
               (else " ")
               ;;; removed printing of issue-type ... 6/14/90
       ;;; pruned this option ...
               (t
                (format nil "~%Not a norm...~%~A ~A"
                        (say *self*
                             (init-stance (list iss side imp (get-node 'aclu group))))
                        type)))))
     issues)))


;; specific code for testing norms
;;
;; E.g.,
;; (map norm-test (collect (db-all issue) issue-norm))
;;
(defun norm-test (obj)  
  (cond ((norm? obj)
         (say (gen-norm-np) obj))
        ((group-p obj)
         (jpn-test-group (gen-pick-one 'pro 'con)
                         (gen-pick-one 'a 'b 'c)
                         (list obj)))
        ((issue-p obj)
         (jpn-test (gen-pick-one 'pro 'con)
                   (gen-pick-one 'a 'b 'c)
                   (list obj)))
        (else nil)))


(defun gen-group-stances (g)  
  (let* ((grp (if (group-p g)
                  g
                  (get-node g group)))
         (stances (group-stances grp)))
    (init-context)
    (paragraph (mapcar #'(lambda (st) (jpn-stance grp st)) stances)
               so)))

    


;;;;------------------------------------------------------------------------
;;;;    say pro/con reasons for issues
;;;;------------------------------------------------------------------------


(defun say-reasons (subj reasons)  
  (paragraph
   (mapcar 
    #'(lambda (reason)
      (say subj reason))
    reasons)
   so))

(defun say-p/c (iss stream)  
  (let* ((i (if (issue-p iss)
                iss
                (get-node iss issue)))
         (pro (issue-pro-stances i))
         (con (issue-con-stances i)))
    (init-context)
    (if pro
        (say-side i 'pro pro stream))
    (if con
        (say-side i 'con con stream))
    (say-opposite iss stream)
    (values)))


(defun say-side (iss side reasons stream)  
  (let ((phrase (case side
                  ((pro) "のサポート ")
                  ((con) "反対 "))))
    (say-cluster-reasons (proto-dnp issue-np
                            'number 'sing
                            'issue t
                            'lex (concatenate 'string phrase (gen-np iss 'obj)))
                         reasons
                         stream)))



(defun say-opposite (iss stream)  
  (cond ((issue-opposite iss)
         (paragraph
          (concatenate 'string 
                       "の反対 "
                       (gen-np iss 'subj)
                       " は "
                       (japanese (get-node (issue-opposite iss) issue))
                       ".")
          stream)))
  (values))

;;;;------------------------------------------------------------------------
;;;;    End of Japanese.lisp
;;;;------------------------------------------------------------------------
