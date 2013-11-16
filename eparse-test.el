
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
     (expect '(fail) (eplib:fail (eplib:make-input 0 "test") 1))

     (desc "一文字だけ取るようなlexer")
     (expect "a"
             (let ((ret (eplex:char '(0 . "a"))))
               (car ret)))
     )
    ))

(ert-run-tests-batch-and-exit)
