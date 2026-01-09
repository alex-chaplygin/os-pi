;; ; Тесты обработчика регулярных выражений
(unit-tests 'regex)

(deftest parse-sym-test ()
  "Тестирование символа"
  (let ((s1 (stream-from-str "a")))
    (print (assertcar (funcall (parse-sym) s1) '(sym #\a)))))

(deftest parse-count-test ()
  "Тестирование количества"
  (let ((s1 (stream-from-str "{12}"))
	(s2 (stream-from-str "{12,}"))
	(s3 (stream-from-str "{12,50}")))
    (print (assertcar (funcall (parse-count) s1) '(repeat 12 ())))
    (print (assertcar (funcall (parse-count) s2) '(repeat 12 more)))
    (print (assertcar (funcall (parse-count) s3) '(repeat 12 50)))))

(deftest parse-range-test ()
  "Тестирование количества"
  (let ((s1 (stream-from-str "[A\\d\\S-b]"))
	(s2 (stream-from-str "[^A]"))
	(s3 (stream-from-str "[A-zB-s]")))
    (print (assertcar (funcall (parse-range) s1) '(RANGE () ((65 ()) (CLASS DIGIT) (CLASS NSPACE) (45 ()) (98 ())))))
    (print (assertcar (funcall (parse-range) s2) '(RANGE NEGATIVE ((65 ())))))
    (print (assertcar (funcall (parse-range) s3) '(RANGE () ((65 122) (66 115)))))))

(deftest parse-sym-test ()
  "Тестирование квантификатора"
  (let ((s1 (stream-from-str "{12}?"))
	(s2 (stream-from-str "+?"))
	(s3 (stream-from-str "*?")))
    (print (assertcar (funcall (parse-quant) s1) '((repeat 12 ()) lazy)))
    (print (assertcar (funcall (parse-quant) s2) '((plus) lazy)))
    (print (assertcar (funcall (parse-quant) s3) '((star) lazy)))))


(deftest parse-element-test ()
  "Тестирование элемента"
  (let ((s1 (stream-from-str "\\+{0,}?"))
	(s2 (stream-from-str "b??"))
	(s3 (stream-from-str "b+"))
	(s4 (stream-from-str ".*?")))
    (print (assertcar (funcall (parse-element) s1) '(lazy-star (sym #\+))))
    (print (assertcar (funcall (parse-element) s2) '(or (epsilon) (sym #\b))))
    (print (assertcar (funcall (parse-element) s3) '(seq (sym #\b) (star (sym #\b)))))
    (print (assertcar (funcall (parse-element) s4) '(lazy-star (any))))))

(deftest parse-group-test ()
  "Тестирование группы"
  (let ((s1 (stream-from-str "(a*)"))
	(s2 (stream-from-str "(b)"))
	(s3 (stream-from-str "((b))")))
    (print (assertcar (funcall (parse-group) s1) '(group (star (sym #\a)))))
    (print (assertcar (funcall (parse-group) s2) '(group (sym #\b))))
    (print (assertcar (funcall (parse-group) s2) '(group (sym #\b))))))

(deftest parse-expr-test ()
  "Тестирование выражения"
  (let ((s1 (stream-from-str "a*b*c"))
	(s2 (stream-from-str "abcd"))
	(s3 (stream-from-str "a"))
	(s4 (stream-from-str "a*(bcd)*"))
	(sor (stream-from-str "a|b|c"))
	(s5 (stream-from-str "a|bc(d*|e)")))
    (print (assertcar (funcall (parse-regex) s1) '(GROUP (SEQ (STAR (SYM #\a))
							(STAR (SYM #\b))
							(SYM #\c)))))
    (print (assertcar (funcall (parse-regex) s2) '(GROUP (SEQ (SYM #\a) (SYM #\b) (SYM #\c) (SYM #\d)))))
    (print (assertcar (funcall (parse-regex) s3) '(GROUP (SYM #\a))))
    (print (assertcar (funcall (parse-regex) sor) '(GROUP (OR (SYM #\a) (SYM #\b) (SYM #\c)))))
    (print (assertcar (funcall (parse-regex) s4)
		      '(GROUP (SEQ (STAR (SYM #\a)) (STAR (GROUP (SEQ (SYM #\b) (SYM #\c) (SYM #\d))))))))
    (print (assertcar (funcall (parse-regex) s5)
		      '(GROUP (OR (SYM #\a) (SEQ (SYM #\b) (SYM #\c) (GROUP (OR (STAR (SYM #\d)) (SYM #\e))))))))))
						

(defun test-auto (regex res)
  "Тест генерации автомата по выражению regex, res - эталонный автомат"
  (reset-state)
  (print (assert (regex-to-nfa (car (funcall (parse-expression) (stream-from-str regex))) nil nil) res)))

;; (deftest test-auto-sym ()
;;   "Генерация автомата: символ в регулярном выражении"
;;   (test-auto "a*" '(1 ((1 #\a (2))) 2)))

;; (deftest test-auto-any ()
;;   "Генерация автомата: любой символ в регулярном выражении"
;;   (test-auto "." '(1 ((1 any (2))) 2)))

;; (deftest test-auto-range ()
;;   "Генерация автомата: любой символ в регулярном выражении"
;;   (test-auto "[^b\\W]" '(1 ((1 any (2))) 2)))

;; (deftest test-auto-star ()
;;   "Генерация автомата: звезда"
;;   (test-auto "b*" '(1 ((3 #\b (4)) (1 E (3)) (4 E (3)) (4 E (2)) (1 E (2))) 2)))

;; (deftest test-auto-lazy-star ()
;;   "Генерация автомата: ленивая звезда"
;;   (test-auto "b*?" '(1 ((3 #\b (4)) (1 E (2)) (4 E (2)) (1 E (3)) (4 E (3))) 2)))

;; (deftest test-auto-question ()
;;   "Генерация автомата: вопрос"
;;   (test-auto "a?" '(1 ((1 E (3)) (3 #\a (4)) (4 E (2)) (1 E (5)) (5 E (6)) (6 E (2))) 2)))

;; (deftest test-auto-plus ()
;;   "Генерация автомата: плюс"
;;   (test-auto "a+" '(3 ((3 #\a (4)) (4 E (5)) (7 #\a (8)) (5 E (7)) (8 E (7)) (8 E (6)) (5 E (6))) 6)))

;; (deftest test-auto-seq ()
;;   "Генерация автомата: последовательность"
;;   (test-auto "abc" '(3 ((3 #\a (4)) (4 E (5)) (5 #\b (6)) (6 E (7)) (7 #\c (8))) 8)))

;; (deftest test-auto-or ()
;;   "Генерация автомата: альтернатива"
;;   (test-auto "a|b|c" '(1 ((1 E (3)) (3 #\a (4)) (4 E (2)) (1 E (5)) (5 #\b (6)) (6 E (2)) (1 E (7)) (7 #\c (8)) (8 E (2))) 2)))



(defun test-match (str regex expected)
  "Поиск первого соответствия строки str строке регулярного выражения regex"
  (let* ((result (match str regex)))
    (if result
	(print (assert (car result) expected) 'STRING str 'REGEX regex  'GROUPS (cdr result))
	(print (assert result expected) 'STRING str 'REGEX regex))))

(defun test-matches (str regex expected)
  "Поиск всех соответствий строки str строке регулярного выражения regex"
  (let* ((result (matches str regex))
	 (number 0))
    (print "-----------------------")
    (print 'STRING str 'REGEX regex)
    (if result
	(labels ((test (result expected)
		   (if (or (null result) (null expected)) nil 
		       (progn (setq number (+ number 1))
			      (print number (assert (caar result) (car expected)) 'GROUPS (cdar result)) 
			      (test (cdr result) (cdr expected))))
		   ))
	  (test result expected))
	(print (assert result expected)))))

(deftest test-regex ()
  "Тестирование регулярных выражений"
  (reset-state)
  (test-match "a" "a{0}" "")
  (test-match "abccccd" "abc{4}d" "abccccd")
  (test-match "assaaabbaa" "a+(s*)(a+(b+)a+)" "assaaabbaa")
  (test-match "test <one> quants <two>" "<(.*?)>" "<one>")
  (test-match "test <one> quants <two>" "<(.*)>" "<one> quants <two>")
  (test-match "abccccd" "abc{2,10}d" "abccccd")
  (test-match "abccccd" "abc{2,}" "abcccc")
  (test-match "abccccd" "abc{2,}?" "abcc")
  (test-match "offsabc4de" "abc4d" "abc4d")
  (test-match "offsabc4d" "abc4d" "abc4d")
  (test-match "abc4daaa" "abc4d" "abc4d")
  (test-match "abc4d" "abc4d" "abc4d")
  (test-match "1a---b 4d_" "b.4d." "b 4d_")
  (test-match "bcdef" "^b..e.$" "bcdef")
  (test-match "abcd" "^bcd" NIL)
  (test-match "abcd" "abc$" NIL)
  (test-match "" ".*" "")
  (test-match "a" "." "a")
  (test-match "aaaatd" "a*t" "aaaat")
  (test-match "aaaat" "a*t" "aaaat")
  (test-match "etd" "a*t" "t")
  (test-match "test" "t.*t" "test")
  (test-match "test2" "t.*es.*t2" "test2")
  (test-match "string" ".*" "string")
  (test-match "taaaabw" "a*b.$" "aaaabw")
  (test-match "test    string 123" "test * str.*2" "test    string 12")
  (test-match "ABCXYZ" "^A.*Z$" "ABCXYZ")
  (test-match "abcktrrrq" ".+ktr+" "abcktrrr")
  (test-match "abcktrrrq" ".+ktr*?" "abckt")
  (test-match "Hello" "Hel+o" "Hello")
  (test-match "heo" "hel+o" NIL)
  (test-match "Helo" "Hel?lo" "Helo")
  (test-match "привет" "при.*т" "привет")
  (test-match "123abcdef123" "\\d*(.)*?\\d+" "123abcdef123")
  (test-match "lisp123*" "\\w+" "lisp123")
  (test-match "123*" "\\d+" "123")
  (test-match "three cool words" ".*\\s(\\w+)\\s.*" "three cool words")
  (test-match "123" "\\D+" NIL)
  (test-match "123" "[^0-3]+" NIL)
  (test-match "^^^" "[\\^]+" "^^^")
  (test-match "abc_123привет" "[^A-Za-z0-9_]+" "привет")
  (test-match ".*?" "\\.\\*\\?" ".*?")
  (test-match "nonnoncapturinggroups" "(?:non)+(?:capturing)(?:groups)" "nonnoncapturinggroups")

  (test-matches "three cool words" "\\w{5}" '("three" "words"))
  (test-matches "three cool words" "^\\w{5}" '("three"))
  (test-matches "three cool words" "^\\w{5}$" '())
  (test-matches "<three> <cool> <words>" "<(.*?)>" '("<three>" "<cool>" "<words>")))
  
(run-tests)
