(unit-tests 'lisp-parser)

; --- Тесты на атомы ---
(deftest simple-symbol-test ()
  "Тест: Простой символ"
  (print (assert (parse-lisp "my-symbol") 'MY-SYMBOL)))
(deftest decimal-number-test ()
  "Тест: Десятичное число"
  (print (assert (parse-lisp "12345") 12345)))
(deftest hexadecimal-number-test ()
  "Тест: Шестнадцатеричное число"
  (print (assert (parse-lisp "0xFF") 255)))
(deftest string-test ()
  "Тест: Строка"
  (print (assert (parse-lisp "\"hello world\"") "hello world")))
(deftest empty-string-test ()
  "Тест: Пустая строка"
  (print (assert (parse-lisp "\"\"") "")))
(deftest character-a-test ()
  "Тест: Символ #\\A"
  (print (assert (parse-lisp "#\\A") #\A)))
(deftest character-space-test ()
  "Тест: Символ #\\Space"
  (print (assert (parse-lisp "#\\ ") #\ )))

; --- Тесты на списки ---
(deftest empty-list-test ()
  "Тест: Пустой список"
  (print (assert (parse-lisp "()") '())))

(deftest simple-lst-test ()
  "Тест: Простой список"
  (print (assert (parse-lisp "(a b c)") '(A B C))))

(deftest numbstr-list-test ()
  "Тест: Список с числами и строками"
  (print (assert (parse-lisp "(1 \"two\" 3)") '(1 "two" 3))))
(deftest nested-list-test ()
  "Тест: Вложенный список"
  (print (assert (parse-lisp "(a (b (c)) d)") '(A (B (C)) D))))
(deftest whitespace-list-test ()
  "Тест: Список с пробелами"
  (print (assert (parse-lisp "  ( a b )  ") '(A B))))

; --- Тесты на специальные формы (Quote, Backquote, и т.д.) ---
(deftest quote-test ()
  "Тест: Quote"
  (print (assert (parse-lisp "'a") '(QUOTE A))))
(deftest quote-list-test ()
  "Тест: Quote списка"
  (print (assert (parse-lisp "'(a b)") '(QUOTE (A B)))))
(deftest double-quote-test ()
  "Тест: Двойной Quote"
  (print (assert (parse-lisp "''a") '(QUOTE (QUOTE A)))))
(deftest backquote-test ()
  "Тест: Backquote"
  (print (assert (parse-lisp "`a") '(BACKQUOTE A))))
(deftest backquote-list-test ()
  "Тест: Backquote со списком"
  (print (assert (parse-lisp "`(a b)") '(BACKQUOTE (A B)))))
(deftest comma-test ()
  "Тест: Comma"
  (print (assert (parse-lisp "`,a") '(BACKQUOTE (COMMA A)))))
(deftest comma-at-test ()
  "Тест: Comma-at"
  (print (assert (parse-lisp "`,@a") '(BACKQUOTE (COMMA-AT A)))))
(deftest complex-backquote-test ()
  "Тест: Сложный Backquote"
  (print (assert (parse-lisp "`(a ,b ,@c)") '(BACKQUOTE (A (COMMA B) (COMMA-AT C))))))
(deftest quote-in-list-test ()
  "Тест: Quote внутри списка"
  (print (assert (parse-lisp "(a 'b)") '(A (QUOTE B)))))
(deftest backquote-in-list-test ()
  "Тест: Backquote внутри списка"
  (print (assert (parse-lisp "(a `(b ,c))") '(A (BACKQUOTE (B (COMMA C)))))))


; --- Тесты на функции ---
(deftest simp-func-test ()
  "Тест: Простая функция"
  (print (assert (parse-lisp "#'my-func") '(FUNCTION MY-FUNC))))
(deftest func-with-args-test ()
  "Тест: Функция с аргументами"
  (print (assert (parse-lisp "#'(a b c)") '(FUNCTION (A B C)))))
(deftest function-with-lambda-test ()
  "Тест: Функция с лямбдой" 
  (print (assert (parse-lisp "#'(lambda (x) (* x x))") '(FUNCTION (LAMBDA (X) (* X X))))))

; --- Тесты на векторы ---
(deftest empty-vector-test ()
  "Тест: Пустой вектор"
  (print (assert (parse-lisp "#()") #())))
(deftest simple-vector-test ()
  "Тест: Простой вектор"
  (print (assert (parse-lisp "#(1 2 3)") #(1 2 3))))
(deftest mixed-vector-test ()
  "Тест: Вектор с разными типами"
  (print (assert (parse-lisp "#(a \"b\" #(c))") #(a "b" #(c)))))

; --- Тесты на точечные пары ---
(deftest simple-dotted-pair-test ()
  "Тест: Простая точечная пара"
  (print (assert (parse-lisp "(a . b)") '(A . B))))
(deftest dotted-pair-in-list-test ()
  "Тест: Точечная пара в конце списка"
  (print (assert (parse-lisp "(a b . c)") '(A B . C))))
(deftest nested-dotted-pair-test ()
  "Тест: Вложенная точечная пара"
  (print (assert (parse-lisp "(a . (b . c))") '(A B . C))))

(defun lex-error-test (expr res)
  (print (assert (handle (parse-list expr) (parse-error (e) e)) res)))

;; --- Тесты на ошибки ---
(deftest lex-test-badstr ()
  "Тест: Незакрытая строка"
  (lex-error-test "\"string" '((1 8)"lisp-lexer: unterminated string"))
  (lex-error-test "\"") '((1 2)"lisp-lexer: unterminated string"))
  (lex-error-test "(1 2 \"hello") '((1 12)"lisp-lexer: unterminated string"))

(deftest lex-test-unknown-token ()
  "Тест: Неизвестная лексема"
  (lex-error-test "@") '((1 1)"lisp-lexer: Unknown token"))

; --- Тест на полную программу. Можно добавить и другие, я решил, что этого хватит ---
;; (deftest full-program-test ()
;;   "Тест: Парсинг полной программы"
;;   (let ((program "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))")
;;         (expected-tree '(DEFUN FACTORIAL (N) (IF (<= N 1) 1 (* N (FACTORIAL (- N 1)))))))
;;     (print (assert (parse-lisp program) expected-tree))))

(run-tests)
