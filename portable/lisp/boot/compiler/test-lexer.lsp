(unit-tests 'lexer)

(deftest lexer-test-parens ()
  "Тест: Скобки"
  (print (assert (lexer "()") '(#\( #\)))))

(deftest lexer-test-integer ()
  "Тест: Целое число"
  (print (assert (lexer "123") '(123))))

(deftest lexer-test-hex ()
  "Тест: 16-ричное число"
  (print (assert (lexer "0xFF") '(255))))

(deftest lexer-test-char ()
  "Тест: Символ (character)"
  (print (assert (lexer (concat (implode (list (code-char 35))) (implode (list (code-char 92))) "q")) '(T-CHAR #\q))))

(deftest lexer-test-float ()
  "Тест: Число с плавающей точкой"
  (print (assert (lexer "12.04 .12") '(12.04 0.12))))

(deftest lexer-test-symbol ()
  "Тест: Идентификатор (symbol)"
  (print (assert (lexer "ab+-*/=_&|<>%!^~") '(AB+-*/=_&|<>%!^~))))

(deftest lexer-test-quote ()
  "Тест: Цитата (')"
  (print (assert (lexer "'VAR") '(QUOTE VAR))))

(deftest lexer-test-backquote ()
  "Тест: Обратная цитата (`)"
  (print (assert (lexer "`(,a ,@b)") '(BACKQUOTE #\( COMMA A COMMA-AT B #\)))))

(deftest lexer-test-string ()
  "Тест: Строка"
  (print (assert (lexer "\"string\"") '("string"))))

(deftest lexer-test-sharp-vector ()
  "Тест: Вектор (#())"
  (print (assert (lexer "#(1)") '(SHARP #\( 1 #\)))))

(deftest lexer-test-dotted-pair ()
  "Тест: Точечная пара"
  (print (assert (lexer "(1 . 2)") '(#\( 1 DOT 2 #\)))))

(deftest lexer-test-function-quote ()
  "Тест: Цитата функции (#')"
  (print (assert (lexer (concat (implode (list (code-char 35))) (implode (list (code-char 39))) "F")) '(FUNCTION F))))

(deftest full-lexer-test ()
  "Полный тест лексера"
  (print (assert (lexer (concat "(123 0xFF "
                     (implode (list (code-char 35) (code-char 92))) "q"
                     " 12.04 .12 ab+-*/=_&|<>%!^~ 'VAR "
                     "`(,a ,@b) "
                     (implode (list (code-char 34))) "string" (implode (list (code-char 34)))
                     " #(1) (1 . 2) "
                     (implode (list (code-char 35) (code-char 39))) "F)"))
          '(#\( 123 255 T-CHAR #\q 12.04 0.12 AB+-*/=_&|<>%!^~ QUOTE VAR BACKQUOTE #\( COMMA A COMMA-AT B #\) "string" SHARP #\( 1 #\) #\( 1 DOT 2 #\) FUNCTION F #\)))))

(run-tests)

