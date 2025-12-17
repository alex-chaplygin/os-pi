(unit-tests 'lexer)

;; Тесты на обработку ошибок

(deftest lex-test-badstr ()
  "Тест: Незакрытая строка"
  (print (assert (handle (lisp-lexer "\"string") (parse-error (x) x)) '((1 8) "lisp-lexer: unterminated string")))
  (print (assert (handle (lisp-lexer "\"") (parse-error (x) x)) '((1 2)"lisp-lexer: unterminated string")))
  (print (assert (handle (lisp-lexer "(1 2 \"hello") (parse-error (x) x)) '((1 12)"lisp-lexer: unterminated string"))))

(deftest lex-test-unknown-token ()
  "Тест: Неизвестная лексема"
  (print (assert (handle (lisp-lexer "@") (parse-error (x) x)) '((1 1)"lisp-lexer: Unknown token"))))

(deftest lex-test-more-errors ()
  "Тест: Дополнительные ошибки лексера"
  (print (assert (handle (lisp-lexer "-.") (parse-error (x) x)) '((1 1)"lisp-lexer: WARNING: got dot with sign")))
  (print (assert (handle (lisp-lexer "\"\\") (parse-error (x) x)) '((1 2)"lisp-lexer: unterminated string or unexpected end of escape sequence")))
  (print (assert (handle (lisp-lexer "\"\\a\"") (parse-error (x) x)) '((1 2)"lisp-lexer: unterminated string or unexpected end of escape sequence"))))

(deftest full-lexer-test ()
  "Полный тест лексера"
  (print (assert (map #'car (lisp-lexer 
		  (concat "(123 0xFF "
			  "#\\q"
			  " 12.0625 .125 13. "
                          "ab+-*/=_&|<>%!^~"
			  "'VAR "
			  "`(,a ,@b) "
			  "\"string\"       "
			  " #(1) (1 . 2) "
			  " #'F)"
			  )))
		 '(#\( 123 255
		   #\q
		   12.0625 0.125 13.0
		   AB+-*/=_&|<>%!^~ 
                   QUOTE VAR BACKQUOTE 
                   #\( COMMA A COMMA-AT B #\) 
                   "string" 
                   SHARP #\( 1 #\) 
                   #\( 1 #\. 2 #\) 
                   FUNCTION F #\)
		   )
		 )))

(run-tests)
