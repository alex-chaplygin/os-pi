(unit-tests 'lisp-parser)

(deftest atoms-parsing-test ()
  "Тест: Атомы — символы, числа, строки"
  (let ((input "(my-symbol 12345 0xFF \"hello world\" \"\" #\\A #\\ )")
        (expected (list 'MY-SYMBOL 12345 255 "hello world" "" #\A #\ )))
    (print (assert (parse-lisp input) expected))))

(deftest lst-dotpair-test ()
  "Тест: Списки и точечные пары"
  (let ((input "(() (a b c) (1 \"two\" 3) (a (b (c)) d) ( a b ) (a . b) (a b . c) (a . (b . c)))")
        (expected (list '()
                        '(A B C)
                        '(1 "two" 3)
                        '(A (B (C)) D)
                        '(A B)
                        '(A . B)
                        '(A B . C)
                        '(A B . C))))
    (print (assert (parse-lisp input) expected))))

(deftest special-forms-test ()
  "Тест: Quote, Backquote, Comma, Comma-At"
  (let ((input "('a '(a b) ''a `a `(a b) `,a `,@a `(a ,b ,@c) (a 'b) (a `(b ,c)))")
        (expected (list
                    '(QUOTE A)
                    '(QUOTE (A B))
                    '(QUOTE (QUOTE A))
                    '(BACKQUOTE A)
                    '(BACKQUOTE (A B))
                    '(BACKQUOTE (COMMA A))
                    '(BACKQUOTE (COMMA-AT A))
                    '(BACKQUOTE (A (COMMA B) (COMMA-AT C)))
                    '(A (QUOTE B))
                    '(A (BACKQUOTE (B (COMMA C)))))))
    (print (assert (parse-lisp input) expected))))

; --- Тесты на функции ---
(deftest func-parsing-test ()
  "Тест: Функции — простая и с аргументами"
  (let ((input "(#'my-func #'(a b c))")
        (expected (list '(FUNCTION MY-FUNC)
                        '(FUNCTION (A B C)))))
    (print (assert (parse-lisp input) expected))))

(deftest function-with-lambda-test ()
  "Тест: Функция с лямбдой" 
  (print (assert (parse-lisp "#'(lambda (x) (* x x))") '(FUNCTION (LAMBDA (X) (* X X))))))

(deftest vectors-parsing-test ()
  "Тест: Векторы — пустой, простой, смешанный"
  (let ((input "(#() #(1 2 3) #(a \"b\" #(c)))")
        (expected (list #()
                        #(1 2 3)
                        #(a "b" #(c)))))
    (print (assert (parse-lisp input) expected))))

;; --- Тесты на ошибки ---
(deftest lex-test-badstr ()
  "Тест: Незакрытая строка"
  (print (assert (parse-lisp "\"string") '((1 8)"lisp-lexer: unterminated string")))
  (print (assert (parse-lisp "\"") '((1 2)"lisp-lexer: unterminated string")))
  (print (assert (parse-lisp "(1 2 \"hello") '((1 12)"lisp-lexer: unterminated string"))))

(deftest lex-test-unknown-token ()
  "Тест: Неизвестная лексема"
  (print (assert (parse-lisp "@") '((1 1)"lisp-lexer: Unknown token"))))

; --- Тесты на полную программу. ---
(deftest full-program-test ()
   "Тест: Парсинг полной программы"
   (let ((program "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))")
         (expected-tree '(DEFUN FACTORIAL (N) (IF (<= N 1) 1 (* N (FACTORIAL (- N 1)))))))
     (print (assert (parse-lisp program) expected-tree))))

(deftest err-program-test ()
  "Тест: Парсинг полной программы с ошибкой (незакрытое описание функции)"
  (print (assert (parse-lisp "(defun factorial (n) \"Вычисляет факториал (if (<= n 1) 1 (* n (factorial (- n 1)))))") '((1 103)"lisp-lexer: unterminated string"))))

(deftest parser-errors-test ()
  "Тест: Ошибки парсера - лишние токены"
  ;(print (assert (parse-lisp "(a (b c)") '((1 8)"lisp-parser: Unbalanced parentheses. Expected a closing parenthesis.")))
  ;(print (assert (parse-lisp "(a b))") '((1 6)"lisp-parser: Found an extra closing parenthesis.")))
  (print (assert (parse-lisp "(a) b") '((1 5)"lisp-parser: Unexpected tokens after expression."))))

(deftest dotted-list-error-test ()
  "Тест: Ошибки в точечных списках"
  (print (assert (parse-lisp "(. a)") '((1 1)"lisp-parser: Invalid dotted list: starts with a dot."))))

(deftest array-error-test ()
  "Тест: Ошибки в массивах"
  (print (assert (parse-lisp "#(a . b)") '((1 1)"lisp-parser: Invalid array: dotted list is not allowed."))))

(run-tests)
