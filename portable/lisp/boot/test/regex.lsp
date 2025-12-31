;; ; Тесты обработчика регулярных выражений
(unit-tests 'regex)
;; (print "-----Regex handler tests-----")

;; (defun test-error (regex ref-error)
;;   (let ((chars (explode (concat "Expected: " ref-error ", got: "))))
;;     (dolist (chr chars)
;;       (putchar chr))
;;     (print (check-correctness regex))))

;; (print "Regex correctness checking tests")
;; (test-error "" "Empty regex")
;; (test-error "t^" "Unescaped ^ not in the beginning of regex")
;; (test-error "$$" "Unescaped $ not in the end of regex")
;; (test-error "ab*?f" "? Preceding token is not quantifiable")
;; (test-error "asd.*[a]* * *+" "+ Preceding token is not quantifiable")
;; (test-error "[ab]+. +%+ +* ?" "* Preceding token is not quantifiable")

;; (test-error "af[]" "Empty square brackets")
;; (test-error "a%[bc]" "Missing opening square bracket")
;; (test-error "^ab[xyz$" "Missing closing square bracket")
;; (test-error "^a[ab[dc]t]&" "Nested square brackets")

;; (print "Regex matching tests")


;; (print (assert (test-re "string" ".*") T))
;; (print (assert (test-re "bcdef" "^b..e.$") T))
;; (print (assert (test-re "abcd" "^bcd") NIL))

(deftest parse-sym-test ()
  "Тестирование символа"
  (let ((s1 (stream-from-str "a")))
    (print (assertcar (funcall (regex-sym) s1) '(sym #\a)))))

(deftest parse-element-test ()
  "Тестирование элемента"
  (let ((s1 (stream-from-str "a*"))
	(s2 (stream-from-str "b"))
	(s3 (stream-from-str ".*")))
    (print (assertcar (funcall (regex-element) s1) '(star (sym #\a))))
    (print (assertcar (funcall (regex-element) s2) '(sym #\b)))
    (print (assertcar (funcall (regex-element) s3) '(star (any))))))

(deftest parse-group-test ()
  "Тестирование группы"
  (let ((s1 (stream-from-str "(a*)"))
	(s2 (stream-from-str "(b)"))
	(s3 (stream-from-str "((b))")))
    (print (assertcar (funcall (regex-group) s1) '(star (sym #\a))))
    (print (assertcar (funcall (regex-group) s2) '(sym #\b)))
    (print (assertcar (funcall (regex-group) s2) '(sym #\b)))))

(deftest parse-expr-test ()
  "Тестирование выражения"
  (let ((s1 (stream-from-str "a*b*c"))
	(s2 (stream-from-str "abcd"))
	(s3 (stream-from-str "a"))
	(s4 (stream-from-str "a*(bcd)*"))
	(sor (stream-from-str "a|b|c"))
	(s5 (stream-from-str "a|bc(d*|e)")))
    (print (assertcar (funcall (regex-expression) s1) '(SEQ (STAR (SYM #\a))
							(STAR (SYM #\b))
							(SYM #\c))))
    (print (assertcar (funcall (regex-expression) s2) '(SEQ (SYM #\a)
							(SYM #\b)
							(SYM #\c)
							(SYM #\d))))
    (print (assertcar (funcall (regex-expression) s3) '(SYM #\a)))
    (print (assertcar (funcall (regex-expression) sor) '(OR (SYM #\a) (SYM #\b) (SYM #\c))))
    (print (assertcar (funcall (regex-expression) s4)
		      '(SEQ (STAR (SYM #\a)) (STAR (SEQ (SYM #\b) (SYM #\c) (SYM #\d))))))
    (print (assertcar (funcall (regex-expression) s5)
		      '(OR (SYM #\a) (SEQ (SYM #\b) (SYM #\c) (OR (STAR (SYM #\d)) (SYM #\e))))))))
						

;; (defun check(auto tape reference)
;;   (let ((result (foldl #'nfa-input auto tape)))
;;     (print (assert (nfa-end result) reference))))

;; (defun debug(auto tape &rest null)
;;   (print `(test ,auto))
;;   (print `(result = ,(nfa-end (foldl #'(lambda (a s)
;; 					 (print `(states ,(nfa-states a) sym ,s))
;; 					 (nfa-input a s))
;; 				     auto tape)))))

;; (deftest test-regex ()
;;   "NFA"
;;   (let* ((s1 (stream-from-str "a*"))
;; 	 (regex (car (funcall (parse-expression) s1)))
;; 	 (nfa (regex-to-nfa regex))
;; 	(auto (make-nfa `(,(car nfa)) (second nfa) `(,(third nfa)))) )
;;     (print regex)
;;     (print nfa)
;;     (debug auto (explode "aaa") T)
;;   ))
(defun test-auto (regex res)
  "Тест генерации автомата по выражению regex, res - эталонный автомат"
  (reset-state)
  (print (assert (regex-to-nfa (car (funcall (regex-expression) (stream-from-str regex)))) res)))

(deftest test-auto-sym ()
  "Генерация автомата: символ в регулярном выражении"
  (test-auto "a" '(1 ((1 #\a (2))) 2)))

(deftest test-auto-any ()
  "Генерация автомата: любой символ в регулярном выражении"
  (test-auto "." '(1 ((1 any (2))) 2)))

(deftest test-auto-star ()
  "Генерация автомата: звезда"
  (test-auto "b*" '(1 ((3 #\b (4)) (1 E (2)) (1 E (3)) (4 E (2)) (4 E (3))) 2)))

(deftest test-auto-seq ()
  "Генерация автомата: последовательность"
  (test-auto "abc" '(3 ((3 #\a (4)) (4 E (5)) (5 #\b (6)) (6 E (7)) (7 #\c (8))) 8)))

(deftest test-auto-or ()
  "Генерация автомата: альтернатива"
  (test-auto "a|b|c" '(1 ((1 E (3)) (3 #\a (4)) (4 E (2)) (1 E (5)) (5 #\b (6)) (6 E (2)) (1 E (7)) (7 #\c (8)) (8 E (2))) 2)))

(defun test-match (str regex)
  "Сопоставление строки str и строки регулярного выражения regex"
  (print str regex)
  (regex-match str regex))

(deftest test-regex ()
  "Тестирование регулярных выражений"
  (print (assert (test-match "abc" "a.c") 't))
  (print (assert (test-match "abc4d" "abc4d") 't))
  (print (assert (test-match "a" ".") 't))
  (print (assert (test-match "" ".") NIL))
  (print (assert (test-match "aaaatd" "a*t") 't))
  (print (assert (test-match "aaaat" "a*t") 't))
  (print (assert (test-match "test" "t.*t") 't))
  (print (assert (test-match "test2" "t.*es.*t2") 't))
  (print (assert (test-match "string" ".*") 't))
  (print (assert (test-match "taaaabw" "a*b.") 't))
  (print (assert (test-match "Hello" "Hel+o") 't))
  (print (assert (test-match "heo" "hel+o") NIL))
  (print (assert (test-match "at" "a?*?t") 't))
  (print (assert (test-match "t" "a?t") 't))
  (print (assert (test-match "apple" "appl?e") 't))
  (print (assert (test-match "aple" "appl?e") NIL))
  (print (assert (test-match "applle" "appl?e") NIL))
  (print (assert (test-match "a" "[a]") 't))
  (print (assert (test-match "b" "[abc]") 't))
  (print (assert (test-match "d" "[abc]") NIL))
  (print (assert (test-match "aaabbbc" "[ab]+c") 't))
  (print (assert (test-match "xaybzc" "x[abc]y[abc]z[abc]") 't)))

(run-tests)
