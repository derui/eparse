
(require 'ert-expectations)
(require 'eparse)

(dont-compile
  (when (fboundp 'expectations)
    (expectations
     (desc "パーサの作成")
     (expect t
             (let ((parser (eparse:make)))
               (eparse:parserp parser))
             )
     (expect nil
             (let (parser)
               (eparse:parserp parser))
             )
     (expect nil (eparse:parserp []))
     (expect nil (eparse:parserp [0 0]))

     (desc "結果を返却する")
     (expect '("t" (1 . "est"))
             (eplib:return (eplib:make-input 0 "test") 1))
     (expect '("" (0 . "test"))
             (eplib:return (eplib:make-input 0 "test") nil))
     (expect '("" (0 . "test"))
             (eplib:return (eplib:make-input 0 "test") -1))
     (expect '("" (0 . "test"))
             (eplib:return (eplib:make-input 0 "test") 0))

     (desc "成功の場合の返却値")
     (expect '(success "t" (1 . "est")) (eplib:success (eplib:make-input 0 "test") 1))
     (desc "失敗の場合の返却値")
     (expect '(fail . "message") (eplib:fail "message"))
     (desc "Either関連の簡単なライブラリ")
     (expect '(t t)
             (list
              (eplib:eitherp (eplib:fail ""))
              (eplib:eitherp (eplib:success (eplib:make-input 0 "test") 1))))
     (expect `(t ,(eplib:fail "message"))
             (let ((newf (eplib:either (eplib:fail "message") nil)))
               (list (boundp 'newf)
                     (funcall newf))))
     (expect `(t ,(eplib:success (eplib:make-input 0 "huga") 0))
             (let ((newf (eplib:either (eplib:success (eplib:make-input 0 "hoge") 1)
                                       (lambda (input) (eplib:success input 0))
                                       )))
               (list (boundp 'newf)
                     (funcall newf (eplib:make-input 0 "huga")))))

     (desc "一文字だけ取るようなlexer")
     (expect "a"
             (let ((ret (eplib:lex char (eplib:make-input 0 "a"))))
               (cadr ret)))
     )
    ))

(ert-run-tests-batch-and-exit)
