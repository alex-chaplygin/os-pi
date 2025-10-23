(unit-tests 'parser)

(deftest parse-elem-test ()
  "Тесты для parse-elem"
  (let ((parse-4-digit (parse-elem #\4))
	(parse-1-digit (parse-elem #\1))
	(s1 (stream-from-str "456"))
	(s2 (stream-from-str "123")))
    (print (assertcar (funcall parse-4-digit s1) #\4))
    (print (assert (funcall parse-4-digit s2) nil))
    (print (assert (funcall parse-1-digit s1) nil))
    (print (assertcar (funcall parse-1-digit s2) #\1))))

(deftest parse-and-test ()
  "Тесты для &&&"
  (let ((s1 (stream-from-str "abcd")))
    (print (assertcar (funcall (&&& (parse-elem #\a) (parse-elem #\b)) s1) '(#\a #\b)))
    (print (assertcar (funcall (&&& (parse-elem #\a)) s1) '(#\a)))
    (print (assert (funcall (&&& (parse-elem #\e) (parse-elem #\b)) s1) nil))
    (print (assert (funcall (&&& (parse-elem #\a) (parse-elem #\a)) s1) nil))
    (print (assertcar (funcall (&&& (parse-elem #\a) (parse-elem #\b) (parse-elem #\c) (parse-elem #\d)) s1)
		   '(#\a #\b #\c #\d)))))

(deftest parse-app-test ()
  "Тесты для parse-app"
  (print (assertcar (funcall (parse-app (&&& (parse-elem #\a) (parse-elem #\b)) #'car) (stream-from-str "abcd")) #\a)))

(deftest parse-or-test ()
  "Тесты для parse-or"
  (let ((s1 (stream-from-str "abc")))
    (print (assertcar (funcall (parse-or (parse-elem #\a) (parse-elem #\b) (parse-elem #\c)) s1) #\a))
    (print (assertcar (funcall (parse-or (parse-elem #\c) (parse-elem #\b) (parse-elem #\a)) s1) #\a))
    (print (assert (funcall (parse-or (parse-elem #\d) (parse-elem #\1)) s1) nil))))

(deftest parse-many-test ()
  "Тесты для parse-many"
  (let ((s1 (stream-from-str "abc1")))
    (print (assertcar (funcall (parse-many #'get-byte) s1) '(#\a #\b #\c #\1)))
    (print (assertcar (funcall (parse-many (parse-pred #'is-alpha)) s1) '(#\a #\b #\c)))
    (print (assertcar (funcall (parse-many (parse-elem #\1)) s1) '()))   ; ← ИСПРАВЛЕНО: assertcar вместо assert
    (print (assertcar (funcall (parse-many (parse-elem #\a)) (stream-from-str "aaaabca")) '(#\a #\a #\a #\a)))
    (print (assertcar (funcall (parse-many (parse-or (parse-elem #\a) (parse-elem #\b))) (stream-from-str "aababca")) '(#\a #\a #\b #\a #\b)))))
 ;; (print (assert (funcall (parse-many (parse-elem 'B)) '(A C B)) '((() . (a c b))))))

;; (deftest parse-some-test ()
;;   "Тесты для parse-some"
;;   (print (assert (funcall (parse-some (parse-elem 'A)) '(A A A A B C A)) '(((a a a a) . (b c a)))))
;;   (print (assert (funcall (parse-some (parse-or (parse-elem 'A) (parse-elem 'B))) '(A A b A B C A)) '(((a a b a b) . (c a)))))
;;   (print (assert (funcall (parse-some (parse-elem 'B)) '(A C B)) nil)))

(deftest parse-pred-test ()
  "Тесты для parse-pred"
  (print (assertcar (funcall (parse-pred #'is-digit) (stream-from-str "1aa")) #\1))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "avc")) nil))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "")) nil)))

(deftest parse-optional-test ()
  "Тесты для комбинатора parse-optional (необязательный парсер)"
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "456")) #\4))
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "123")) nil)))

;; (deftest parse-many-sep-test ()
;;   "Тесты для parse-many-sep"
;;   (print "parse-many-sep")
;;   (let ((parser (parse-many-sep (parse-elem 1) (parse-elem 0))))
;;     (print (assert (funcall parser '(1 0 1 0 1 2)) '(((1 1 1) . (2)))))
;;     (print (assert (funcall parser '(2 1 0 1)) '((() . (2 1 0 1)))))
;;     (print (assert (funcall parser '()) '((() . ()))))))

;; (deftest parse-some-sep-test ()
;;   "Тесты для parse-some-sep"
;;   (print "parse-some-sep")
;;   (let ((parser (parse-some-sep (parse-elem 1) (parse-elem 0))))
;;     (print (assert (funcall parser '(1 0 1 2)) '(((1 1) . (2)))))
;;     (print (assert (funcall parser '(2 1 0 1)) nil))
;;     (print (assert (funcall parser '()) nil))
;;     (print (assert (funcall parser '(1)) '(((1) . ()))))))

(deftest parse-decimal-test ()
  "Тесты для parse-decimal"
  (print (assertcar (funcall (parse-decimal) (stream-from-str "123")) 123))
  (print (assertcar (funcall (parse-decimal) (stream-from-str "-456")) -456))
  (print (assert (funcall (parse-decimal) (stream-from-str "abc")) nil))
  (print (assertcar (funcall (parse-decimal) (stream-from-str "0")) 0)))

(deftest parse-hex-test ()
  "Тесты для parse-hex"
  (print (assertcar (funcall (parse-hex) (stream-from-str "0xFF")) 255))
  (print (assertcar (funcall (parse-hex) (stream-from-str "0x0")) 0))
  (print (assertcar (funcall (parse-hex) (stream-from-str "0xABC")) 2748))
  (print (assert (funcall (parse-hex) (stream-from-str "0xG")) nil))
  (print (assert (funcall (parse-hex) (stream-from-str "123")) nil)))

(deftest parse-tnumber-test ()
  "Тесты для parse-tnumber (поддерживает decimal и hex)"
  (print (assertcar (funcall (parse-tnumber) (stream-from-str "123")) 123))
  (print (assertcar (funcall (parse-tnumber) (stream-from-str "0xFF")) 255))
  (print (assertcar (funcall (parse-tnumber) (stream-from-str "-42")) -42))
  (print (assert (funcall (parse-tnumber) (stream-from-str "abc")) nil)))

(deftest parse-tsymbol-test ()
  "Тесты для parse-tsymbol"
  (print (assertcar (funcall (parse-tsymbol) (stream-from-str "ABCD")) 'abcd))
  (print (assertcar (funcall (parse-tsymbol) (stream-from-str "-A_b+3&%")) '-A_b+3&%))
  (print (assertcar (funcall (parse-tsymbol) (stream-from-str "x123")) 'x123))
  (print (assert (funcall (parse-tsymbol) (stream-from-str "123")) nil)) ; символ не может начинаться с цифры
  (print (assert (funcall (parse-tsymbol) (stream-from-str "\"abc\"")) nil)))

(deftest parse-tchar-test ()
  "Тесты для parse-tchar"
  (print (assertcar (funcall (parse-tchar) (stream-from-str "#\\A")) #\A))
  (print (assertcar (funcall (parse-tchar) (stream-from-str "#\\ ")) #\ ))
  (print (assertcar (funcall (parse-tchar) (stream-from-str "#\\newline")) #\newline))
  (print (assert (funcall (parse-tchar) (stream-from-str "A")) nil))
  (print (assert (funcall (parse-tchar) (stream-from-str "#\\")) nil)))

(deftest parse-tfunction-test ()
  "Тесты для parse-tfunction"
  (print (assertcar (funcall (parse-tfunction) (stream-from-str "#'f")) '(function f)))
  (print (assertcar (funcall (parse-tfunction) (stream-from-str "#'(lambda () nil)")) '(function (lambda () nil))))
  (print (assert (funcall (parse-tfunction) (stream-from-str "'f")) nil))
  (print (assert (funcall (parse-tfunction) (stream-from-str "#f")) nil)))

(deftest parse-tstring-test ()
  "Тесты для parse-tstring"
  (print (assertcar (funcall (parse-tstring) (stream-from-str "\"abcde\"")) "abcde"))
  (print (assertcar (funcall (parse-tstring) (stream-from-str "\"\"")) ""))
  (print (assertcar (funcall (parse-tstring) (stream-from-str "\"a\\\"b\"")) "a\"b")) ; если поддерживается экранирование
  (print (assert (funcall (parse-tstring) (stream-from-str "abc")) nil))
  (print (assert (funcall (parse-tstring) (stream-from-str "\"unterminated")) nil)))

(deftest parse-atom-test ()
  "Тесты для parse-atom (символ, число, строка, char, функция и т.д.)"
  (print (assertcar (funcall (parse-atom) (stream-from-str "ABCD")) 'abcd))
  (print (assertcar (funcall (parse-atom) (stream-from-str "123")) 123))
  (print (assertcar (funcall (parse-atom) (stream-from-str "0xFF")) 255))
  (print (assertcar (funcall (parse-atom) (stream-from-str "#\\A")) #\A))
  (print (assertcar (funcall (parse-atom) (stream-from-str "\"hi\"")) "hi"))
  (print (assertcar (funcall (parse-atom) (stream-from-str "#'f")) '(function f))))

(deftest parse-list-test ()
  "Тесты для parse-list"
  (print (assertcar (funcall (parse-list) (stream-from-str "(A B C D)")) '(a b c d)))
  (print (assertcar (funcall (parse-list) (stream-from-str "()")) '()))
  (print (assertcar (funcall (parse-list) (stream-from-str "(123 (ABC #\\D) \"string\" (0xABC))"))
                    '(123 (abc #\D) "string" (2748))))
  (print (assertcar (funcall (parse-list) (stream-from-str "(((A)) B C (F (D)))"))
                    '(((a)) b c (f (d)))))
  (print (assert (funcall (parse-list) (stream-from-str "A B)")) nil)))

(deftest parse-tarray-test ()
  "Тесты для parse-tarray"
  (print (assertcar (funcall (parse-tarray) (stream-from-str "#(1 2 3)")) #(1 2 3)))
  (print (assertcar (funcall (parse-tarray) (stream-from-str "#()")) #()))
  (print (assertcar (funcall (parse-tarray) (stream-from-str "#(0xFF \"a\")")) #(255 "a")))
  (print (assert (funcall (parse-tarray) (stream-from-str "(1 2 3)")) nil)))

(deftest parse-lisp-test ()
  "Тесты для основного парсера parse-lisp (или parse)"
  (print (assert (parse-lisp "  ABCD") 'abcd))
  (print (assert (parse-lisp "(A B C D)") '(a b c d)))
  (print (assert (parse-lisp "(  )") '()))
  (print (assert (parse-lisp "(((A)) B C (F (D)))") '(((a)) b c (f (d)))))
  (print (assert (parse-lisp "(123 (ABC #\\D) \"string\" (0xABC))")
                 '(123 (abc #\D) "string" (2748))))
  (print (assert (parse-lisp "#(1 2 3)") #(1 2 3)))
  (print (assert (parse-lisp "#'f") '(function f))))

(run-tests)
