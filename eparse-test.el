
(require 'cl-lib)
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

      (desc "入力値の管理")
      (expect t (eplib:sourcep (eplib:make-source "test")))

      (desc "位置情報を管理する")
      (expect t (eplib:posp (eplib:make-pos)))
      (expect 0 (eplib:pos-position (eplib:make-pos)))
      (expect 0 (eplib:pos-col (eplib:make-pos)))
      (expect 0 (eplib:pos-line (eplib:make-pos)))
      (expect '(7 2 5)
        (let* ((pos (eplib:make-pos))
               (pos (eplib:update-pos pos 7 '((col . 2)
                                              (line . 5)))))
          (list (eplib:pos-position pos)
                (eplib:pos-col pos)
                (eplib:pos-line pos))))
      (expect '(6 0 5)
        (let* ((pos (eplib:make-pos))
               (pos (eplib:update-pos pos 6 '((line . 5)))))
          (list (eplib:pos-position pos)
                (eplib:pos-col pos)
                (eplib:pos-line pos))))
      (expect '(8 2 0)
        (let* ((pos (eplib:make-pos))
               (pos (eplib:update-pos pos 8 '((col . 2)))))
          (list (eplib:pos-position pos)
                (eplib:pos-col pos)
                (eplib:pos-line pos))))
      (desc "inputからの読出し")
      (expect "t"
        (eplib:read-from-source (eplib:make-source "test") 1))
      (expect '("t" "t")
        (let ((input (eplib:make-source "test")))
          (list (eplib:read-from-source input 1)
                (eplib:read-from-source input 1))))
      (desc "空文字列から読出した時にはエラーが発生する")
      (expect t
              (condition-case foo
                  (eplib:read-from-source (eplib:make-source "") 1)
                (eplib:eof t)))

      (desc "成功の場合の返却値")
      (expect '(success 1)
        (let ((ret (eplib:success (eplib:make-source "hoge") 1)))
          (list (car ret) (cadr ret))))
      (expect "foobar"
        (cadr (eplib:success (eplib:make-source "test") "foobar")))
      (expect t
        (eplib:successp (eplib:success (eplib:make-source "test") "foobar")))
      (expect "foobar"
        (eplib:success-val (eplib:success (eplib:make-source "test") "foobar")))
      (expect "a"
        (eplib:source-text (eplib:pull-source (eplib:success (eplib:make-source "a") ""))))

      (desc "失敗の場合の返却値")
      (expect t (eplib:failp (eplib:fail "message")))
      (expect "message" (eplib:fail-val (eplib:fail "message")))

      (desc "関数同士の合成")
      (expect "te"
        (let ((f (eplib:bind #'(lambda (arg) (concat arg "t"))
                             #'(lambda (arg) (concat arg "e")))))
          (funcall f "")))
      (expect '(t "hoge")
        (let ((f (eplib:bind #'(lambda (arg) (concat arg "t"))
                             nil)))
          (list (eq 'identity f)
                (funcall f "hoge"))))
      (expect '(t "hoge")
        (let ((f (eplib:bind nil
                             #'(lambda (arg) (concat arg "t")))))
          (list (eq 'identity f)
                (funcall f "hoge"))))

      (desc "lexer作成用のマクロ")
      (expect '(t "a" "a" 1)
        (eplib:define-lexer test source rest
                            (let (ch)
                              (setq ch (<<- 1))
                              (forward-pos 1)
                              (success ch)))
        (let ((ret (eplib:lexer:test (eplib:make-source "a"))))
          (list (eplib:successp ret)
                (eplib:success-val ret)
                (eplib:source-text (eplib:pull-source ret))
                (eplib:pos-position (eplib:source-pos (eplib:pull-source ret))))))
      (desc "lexerの結合を行なうためのマクロ")
      (expect "a s"
        (cl-letf ((lex (eplib:lex
                    (let (ch)
                      (setq ch (<- 'any-char))
                      (setq ch (concat ch " " (<- 'char ?s)))
                      (success ch)))))
          (funcall lex (eplib:make-source "as"))))
      (desc "lexer内でエラーが発生したらfailになる")
      (expect t
        (eplib:failp (eplib:lexer:char (eplib:make-source "") ?a)))
      (desc "パーサ内で利用できるコンビネーターを定義できる")
      (expect "abcd"
              (eplib:define-combinator test (lexer)
                                       (let (ch)
                                         (setq ch (<- lexer))
                                         (setq ch (concat ch (<- lexer)))
                                         (setq ch (concat ch (<- lexer)))
                                         (setq ch (concat ch (<- lexer)))
                                         (success ch)))
              (funcall (eplib:lex
                        (success (<$> 'test 'any-char)))
                       (eplib:make-source "abcd")))
      )))

(ert-run-tests-batch-and-exit)
