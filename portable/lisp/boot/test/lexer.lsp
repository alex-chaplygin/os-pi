(unit-tests 'lexer)

(deftest lexer-test-parens ()
  "Тест: Скобки"
  (let ((str "()"))
    (let ((res '(#\( #\))))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-integer ()
  "Тест: Целое число"
  (let ((str "123"))
    (let ((res '(123)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-hex ()
  "Тест: 16-ричное число"
  (let ((str "0xFF"))
    (let ((res '(255)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-char ()
  "Тест: Символ (character)"
  (let ((str (concat (implode (list (code-char 35))) (implode (list (code-char 92))) "q")))
    (let ((res '(T-CHAR #\q)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-float ()
  "Тест: Число с плавающей точкой"
  (let ((str "12.04 .12"))
    (let ((res '(12.04 0.12)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-symbol ()
  "Тест: Идентификатор (symbol)"
  (let ((str "ab+-*/=_&|<>%!^~"))
    (let ((res '(AB+-*/=_&|<>%!^~)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-quote ()
  "Тест: Цитата (')"
  (let ((str "'VAR"))
    (let ((res '(QUOTE VAR)))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-backquote ()
  "Тест: Обратная цитата (`)"
  (let ((str "`(,a ,@b)"))
    (let ((res '(BACKQUOTE #\( COMMA A COMMA-AT B #\))))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-string ()
  "Тест: Строка"
  (let ((str (concat (implode (list (code-char 34))) "string" (implode (list (code-char 34))))))
    (let ((res '("string")))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-sharp-vector ()
  "Тест: Вектор (#())"
  (let ((str "#(1)"))
    (let ((res '(SHARP #\( 1 #\))))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-dotted-pair ()
  "Тест: Точечная пара"
  (let ((str "(1 . 2)"))
    (let ((res '(#\( 1 DOT 2 #\))))
      (print (assert (lexer str) res))))) 

(deftest lexer-test-function-quote ()
  "Тест: Цитата функции (#')"
  (let ((str (concat (implode (list (code-char 35))) (implode (list (code-char 39))) "F")))
    (let ((res '(FUNCTION F)))
      (print (assert (lexer str) res))))) 

(deftest full-lexer-test ()
  "Полный тест лексера"
  (let ((str (concat "(123 0xFF "
                     (implode (list (code-char 35) (code-char 92))) "q"
                     " 12.04 .12 ab+-*/=_&|<>%!^~ 'VAR "
                     "`(,a ,@b) "
                     (implode (list (code-char 34))) "string" (implode (list (code-char 34)))
                     " #(1) (1 . 2) "
                     (implode (list (code-char 35) (code-char 39))) "F)")))
    (let ((res '(#\( 123 255 T-CHAR #\q 12.04 0.12 AB+-*/=_&|<>%!^~ QUOTE VAR BACKQUOTE #\( COMMA A COMMA-AT B #\) "string" SHARP #\( 1 #\) #\( 1 DOT 2 #\) FUNCTION F #\))))
      ;(print "Полный тест: результат лексера:")
      ;(print (lexer str))
      ;(print "Полный тест: ожидаемый результат:")
      ;(print res)
      (print (assert (lexer str) res)))))

(run-tests)
