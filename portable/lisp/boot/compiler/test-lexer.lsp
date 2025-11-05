(unit-tests 'lexer)

(deftest lex-test-parens ()
  "Тест: Скобки"
  (print (assert (lexer "()") '(#\( #\)))))

(deftest lex-test-int ()
  "Тест: Целое число"
  (print (assert (lexer "123") '(123)))
  (print (assert (lexer "007") '(7))))

(deftest lex-test-hex ()
  "Тест: 16-ричное число"
  (print (assert (lexer "0xFF") '(255))))

(deftest lex-test-char ()
  "Тест: Символ (character)"
  (print (assert (lexer "#\\q") '(T-CHAR #\q))))

(deftest lex-test-float ()
  "Тест: Число с плавающей точкой"
  (print (assert (lexer "12.04 .12") '(12.040000 0.120000))))

(deftest lex-test-sym ()
  "Тест: Идентификатор (symbol)"
  (print (assert (lexer "ab+-*/=_&|<>%!^~") '(AB+-*/=_&|<>%!^~))))

(deftest lex-test-quote ()
  "Тест: Цитата (')"
  (print (assert (lexer "'VAR") '(QUOTE VAR))))

(deftest lex-test-backq ()
  "Тест: Обратная цитата (`)"
  (print (assert (lexer "`(,a ,@b)") '(BACKQUOTE #\( COMMA A COMMA-AT B #\)))))

(deftest lex-test-str ()
  "Тест: Строка"
  (print (assert (lexer "\"string\"") '("string")))
  (print (assert (lexer "\"\"") '(""))))

(deftest lex-test-sharp-vec ()
  "Тест: Вектор (#())"
  (print (assert (lexer "#(1)") '(SHARP #\( 1 #\)))))

(deftest lex-test-dot ()
  "Тест: Точечная пара"
  (print (assert (lexer "(1 . 2)") '(#\( 1 DOT 2 #\)))))

(deftest lex-test-func-q ()
  "Тест: Цитата функции (#')"
  (print (assert (lexer "#'F") '(FUNCTION F))))

(deftest lex-test-comments ()
  "Тест: Комментарии"
  (print (assert (lexer "; comment\n42") '(42))))

(deftest lex-test-invalid ()
  "Тест: Некорректные токены"
  (let ((*parse-errors* nil))
    (print (assert (lexer "\"str") nil)) ; Незакрытая строка
    (print (assert (lexer "123a") (list (intern "123A"))))))

(deftest lex-test-full ()
  "Полный тест лексера"
  (let ((input-str "(123 0xFF #\\q 12.04 .12 ab+-*/=_&|<>%!^~ 'VAR `(,a ,@b) \"string\" #(1) (1 . 2)  #'F)")
        (expected-res '(#\( 123 255 T-CHAR #\q 12.040000 0.120000 AB+-*/=_&|<>%!^~ QUOTE VAR BACKQUOTE #\( COMMA A COMMA-AT B #\) "string" SHARP #\( 1 #\) #\( 1 DOT 2 #\) FUNCTION F #\))))
    (print (assert (lexer input-str) expected-res))))

(run-tests)