(unit-tests 'lexer)

(defun strip-token-positions (tokens)
  (map #'car tokens))

(deftest lex-test-parens ()
  "Тест: Скобки"
  (print (assert (strip-token-positions (lisp-lexer "()")) '(#\( #\)))))

(deftest lex-test-int ()
  "Тест: Целое число"
  (print (assert (strip-token-positions (lisp-lexer "123")) '(123)))
  (print (assert (strip-token-positions (lisp-lexer "007")) '(7))))

(deftest lex-test-hex ()
  "Тест: 16-ричное число"
  (print (assert (strip-token-positions (lisp-lexer "0xFF")) '(255))))

(deftest lex-test-char ()
  "Тест: Символ (character)"
  (print (assert (strip-token-positions (lisp-lexer "#\\q")) '(#\q))))

(deftest lex-test-float ()
  "Тест: Число с плавающей точкой"
  (print (assert (strip-token-positions (lisp-lexer "12.04 .12")) '(12.040000 0.120000))))

(deftest lex-test-sym ()
  "Тест: Идентификатор (symbol)"
  (print (assert (strip-token-positions (lisp-lexer "ab+-*/=_&|<>%!^~")) '(AB+-*/=_&|<>%!^~))))

(deftest lex-test-quote ()
  "Тест: Цитата (')"
  (print (assert (strip-token-positions (lisp-lexer "'VAR")) '(QUOTE VAR))))

(deftest lex-test-backq ()
  "Тест: Обратная цитата (`)"
  (print (assert (strip-token-positions (lisp-lexer "`(,a ,@b)")) '(BACKQUOTE #\( COMMA A COMMA-AT B #\)))))

(deftest lex-test-str ()
  "Тест: Строка"
  (print (assert (strip-token-positions (lisp-lexer "\"string\"")) '("string")))
  (print (assert (strip-token-positions (lisp-lexer "\"\"")) '(""))))

(deftest lex-test-sharp-vec ()
  "Тест: Вектор (#())"
  (print (assert (strip-token-positions (lisp-lexer "#(1)")) '(SHARP #\( 1 #\)))))

(deftest lex-test-dot ()
  "Тест: Точечная пара"
  (print (assert (strip-token-positions (lisp-lexer "(1 . 2)")) '(#\( 1 #\. 2 #\)))))

(deftest lex-test-func-q ()
  "Тест: Цитата функции (#')"
  (print (assert (strip-token-positions (lisp-lexer "#'F")) '(FUNCTION F))))

(deftest lex-test-comments ()
  "Тест: Комментарии"
  (print (assert (lisp-lexer "; comment") '())))

;; Тесты на обработку ошибок

(deftest lex-test-badstr ()
  "Тест: Незакрытая строка"
  (print (assert (catch 'parse-error (lisp-lexer "\"string")) "lisp-lexer: unterminated string or unexpected end of escape sequence"))
  (print (assert (catch 'parse-error (lisp-lexer "\"")) "lisp-lexer: unterminated string or unexpected end of escape sequence"))
  (print (assert (catch 'parse-error (lisp-lexer "(1 2 \"hello")) "lisp-lexer: unterminated string or unexpected end of escape sequence")))

(deftest lex-test-unknown-token ()
  "Тест: Неизвестная лексема"
  (print (assert (catch 'parse-error (lisp-lexer "@")) "lisp-lexer: Unknown token")))

(deftest lex-test-more-errors ()
  "Тест: Дополнительные ошибки лексера"
  (print (assert (catch 'parse-error (lisp-lexer "-.")) "lisp-lexer: WARNING: got dot with sign"))
  (print (assert (catch 'parse-error (lisp-lexer "\"\\")) "lisp-lexer: unterminated string or unexpected end of escape sequence"))
  (print (assert (catch 'parse-error (lisp-lexer "\"\\a\"")) "lisp-lexer: unterminated string or unexpected end of escape sequence")))

(deftest full-lexer-test ()
  "Полный тест лексера"
  (print (assert (strip-token-positions (lisp-lexer 
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
