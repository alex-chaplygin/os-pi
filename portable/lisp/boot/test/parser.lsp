(defun test-parse-elem ()
  (let ((parse-4-digit (parse-elem 4))
	(parse-1-digit (parse-elem 1)))
    (print (assert (funcall parse-4-digit '(4 5 6)) '((4 5 6))))
    (print (assert (funcall parse-4-digit '(1 2 3)) nil))
    (print (assert (funcall parse-1-digit '(4 5 6)) nil))
    (print (assert (funcall parse-1-digit '(1 2 3)) '((1 2 3))))))

(defun test-parse-&&& ()
  (print (assert (funcall (&&& #'(lambda (syms) (cons (nth syms 0) (nth syms 1))) (parse-elem 'A) (parse-elem 'B)) '(A B C D)) '(((A . B) . (C D)))))
  (print (assert (funcall (&&& #'(lambda (syms) (cons (nth syms 0) (nth syms 1))) (parse-elem 'A) (parse-elem 'B)) '(A C B)) nil))
  (print (assert (funcall (&&& #'(lambda (syms) (cons (nth syms 0) (nth syms 1))) (parse-elem 'A) (parse-elem 'B)) '(B C B)) nil))
  (print (assert (funcall (&&& #'(lambda (syms) (cons (nth syms 0) (nth syms 1))) (parse-elem 'B) (parse-elem 'C)) '(B C A)) '(((B . C) . (A)))))
  (print (assert (funcall (&&& #'(lambda (syms) (list (nth syms 0) (nth syms 1))) (parse-elem 'B) (parse-elem 'C)) '(B C A)) '(((B C) . (A)))))
  (print (assert (funcall (&&& #'(lambda (syms) (cons (nth syms 0) (nth syms 1))) (parse-elem 'B) (parse-elem 'C)) '(B C A A A D)) '(((B . C) . (A A A D)))))
  (print (assert (funcall (&&& #'(lambda (syms) (list (nth syms 0) (nth syms 1) (nth syms 2))) (parse-elem 'A) (parse-elem 'B) (parse-elem 'C)) '(A B C D)) '(((A B C) . (D))))))

(defun test-parse-parse-or ()
  (print (assert (funcall (parse-or (parse-elem 'A) (parse-elem 'B) (parse-elem 'C)) '(A B C)) '((a . (b c)))))
  (print (assert (funcall (parse-or (parse-elem 'C) (parse-elem 'B) (parse-elem 'D)) '(B C D)) '((b . (c d)))))
  (print (assert (funcall (parse-or (parse-elem 'C) (parse-elem 'B) (parse-elem '1)) '(1 C)) '((1 . (c)))))
  (print (assert (funcall (parse-or (parse-elem 'A) (parse-elem 'B)) '(C B B)) nil)))

(defun test-parse-many ()
  (print (assert (funcall (parse-many (parse-elem 'A)) '(A A A A B C A)) '(((a a a a) . (b c a)))))
  (print (assert (funcall (parse-many (parse-or (parse-elem 'A) (parse-elem 'B))) '(A A b A B C A)) '(((a a b a b) . (c a)))))
  (print (assert (funcall (parse-many (parse-elem 'B)) '(A C B)) '((() . (a c b))))))

(defun test-parse-pred ()
  (print (assert (funcall (parse-pred #'is-digit) '(#\1 B C)) '((#\1 . (b c)))))
  (print (assert (funcall (parse-pred #'is-alpha) '(#\a 1 B C)) '((#\a . (1 b c))))))

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

(defun test-parse (str exp)
  "Тест парсера, str - входная строка, exp - ожидаемое выражение"
  (print (assert (parse-lisp str) exp)))

(test-parse-elem)
(test-parse-&&&)
(test-parse-parse-or)
(test-parse-many)
(test-parse-pred)
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
            '(123 (abc #\D) "string" (2748)))
