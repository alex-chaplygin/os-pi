(defun test-parse-decimal (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-decimal) (explode str))) exp)))

(defun test-parse-hex (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-hex) (explode str))) exp)))

(defun test-parse-tnumber (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tnumber) (explode str))) exp)))

(defun test-parse-tsymbol (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tsymbol) (explode str))) exp)))

(defun test-parse-tchar (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tchar) (explode str))) exp)))

(defun test-parse-tfunction (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tfunction) (explode str))) exp)))

(defun test-parse-tstring (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tstring) (explode str))) exp)))

(defun test-parse-atom (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-atom) (explode str))) exp)))

(defun test-parse-list (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-list) (explode str))) exp)))

(defun test-parse-tarray (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (caar (funcall (parse-tarray) (explode str))) exp)))

(defun test-parse (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (parse-lisp str) exp)))

(test-parse-atom "  ABCD" 'abcd)
(test-parse-atom "AB" 'ab)
(test-parse-list "  (A   B C     D   )" '(a b c d))
(test-parse "  ABCD" 'abcd)
(test-parse "(A B C D)" '(a b c d))
(test-parse "(  )" '())
(test-parse "(((A)) B C (F (D)))" '(((A)) B C (F (D))))
(test-parse-decimal "123" 123)
(test-parse-hex "0xFF" 255)
(test-parse-tnumber "123" 123)
(test-parse-tnumber "0xFF" 255)
(test-parse-tnumber "0xFFABOBA" 65451)
(test-parse-tsymbol "-A_b+3&%" '-A_b+3&%)
(test-parse-tchar "#\\A" #\A)
(test-parse-tfunction "#'f" '(function f))
(test-parse-tfunction "#'(lambda () nil)" '(function (lambda () nil)))
(test-parse-tstring "\"abcde\"" "abcde")
(test-parse-tstring "\"\"" "")
(test-parse "(123 (ABC #\\D) \"string\" (0xABC))"
(test-parse (concat "(123 (ABC #\\D) " str-quote "string" str-quote " (0xABC))")
            '(123 (abc #\D) "string" (2748)))
(test-parse-tarray "#(1 2 3)" #(1 2 3))
