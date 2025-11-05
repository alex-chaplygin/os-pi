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
  (let ((s1 (stream-from-str "abc"))
        (*parse-errors* nil))
    (print (assertcar (funcall (parse-or (parse-elem #\a) (parse-elem #\b)) s1) #\a))
    (print (assertcar (funcall (parse-or (parse-elem #\c) (parse-elem #\b) (parse-elem #\a)) s1) #\a))
    (let ((failing-parser (parse-or (parse-elem #\d) (parse-elem #\1))))
      (print (assert (funcall failing-parser s1) nil))
      (print (assert (car *parse-errors*) "parse-or: All alternatives failed")))))

(deftest parse-many-test ()
  "Тесты для parse-many"
  (let ((s1 (stream-from-str "abc1")))
    (print (assertcar (funcall (parse-many #'get-byte) s1) '(#\a #\b #\c #\1)))
    (print (assertcar (funcall (parse-many (parse-pred #'is-alpha)) s1) '(#\a #\b #\c)))
    (print (assertcar (funcall (parse-many (parse-elem #\1)) s1) '()))
    (print (assertcar (funcall (parse-many (parse-elem #\a)) (stream-from-str "aaaabca")) '(#\a #\a #\a #\a)))
    (print (assertcar (funcall (parse-many (parse-or (parse-elem #\a) (parse-elem #\b))) (stream-from-str "aababca")) '(#\a #\a #\b #\a #\b)))
  ))

(deftest parse-some-test ()
  "Тесты для parse-some"
  (let ((s1 (stream-from-str "aac1"))
	(s2 (stream-from-str "baac1"))
        (*parse-errors* nil))
    (print (assertcar (funcall (parse-some (parse-elem #\a)) s1) '(#\a #\a)))
    (print (assertcar (funcall (parse-some (parse-elem #\b)) s2) '(#\b)))
    (print (assert (funcall (parse-some (parse-elem #\c)) s2) nil))
    (print (assert (car *parse-errors*) "parse-some: Expected at least one occurrence"))))

(deftest parse-pred-test ()
  "Тесты для parse-pred"
  (print (assertcar (funcall (parse-pred #'is-digit) (stream-from-str "1aa")) #\1))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "avc")) nil))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "")) nil)))

(deftest parse-optional-test ()
  "Тесты для комбинатора parse-optional (необязательный парсер)"
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "456")) #\4))
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "123")) nil)))

(deftest parse-decimal-test ()
  "Тесты для parse-decimal"
  (print "Тестирование parse-decimal со стандартными разделителями (без аргументов)...")
  (print (assertcar (funcall (parse-decimal) (stream-from-str "123")) 123))
  (print (assertcar (funcall (parse-decimal) (stream-from-str "45 ")) 45))
  (print (assertcar (funcall (parse-decimal) (stream-from-str (concat "67" (implode (list (code-char 10)))))) 67))
  (print (assertcar (funcall (parse-decimal) (stream-from-str "-100")) -100))
  
  ;; --- Тесты на границы ---
  (print (assertcar (funcall (parse-decimal) (stream-from-str "2147483647")) 2147483647)) ; Максимальное значение
  (print (assertcar (funcall (parse-decimal) (stream-from-str "-2147483648")) -2147483648)) ; Минимальное значение
  (print (assert (catch 'parse-error (funcall (parse-decimal) (stream-from-str "123a"))) "parse-decimal: Number not followed by a delimiter or end-of-stream"))
  (print (assert (catch 'parse-error (funcall (parse-decimal) (stream-from-str "2147483648"))) "parse-decimal: Invalid integer format or overflow")) ; Положительное переполнение
  (print (assert (catch 'parse-error (funcall (parse-decimal) (stream-from-str "-2147483649"))) "parse-decimal: Invalid integer format or overflow")) ; Отрицательное переполнение

  (print "Тестирование parse-decimal с кастомными разделителями (с аргументами)...")
  (let ((custom-delims (list #\; #\,)))
    (let ((parser (parse-decimal custom-delims)))
      (print (assertcar (funcall parser (stream-from-str "987;")) 987))
      (print (assertcar (funcall parser (stream-from-str "-10,")) -10))
      (print (assert (catch 'parse-error (funcall parser (stream-from-str "55 "))) "parse-decimal: Number not followed by a delimiter or end-of-stream")))))

(deftest parse-hex-test ()
  "Тесты для parse-hex"
  (print "Тестирование hex parser со стандартными разделителями (без аргументов)...")
  (print (assertcar (funcall (parse-hex) (stream-from-str "0xF")) 15))
  (print (assertcar (funcall (parse-hex) (stream-from-str "0xAB ")) 171))
  
  ;; --- Тесты на границы ---
  (print (assertcar (funcall (parse-hex) (stream-from-str "0x7FFFFFFF")) 2147483647)) ; Максимальное значение
  (print (assert (catch 'parse-error (funcall (parse-hex) (stream-from-str "0xFg"))) "parse-hex: Number not followed by a delimiter or end-of-stream"))
  (print (assert (catch 'parse-error (funcall (parse-hex) (stream-from-str "0xFFFFFFFF"))) "parse-hex: Invalid hexadecimal format or overflow")) ; Положительное переполнение
  (print (assert (catch 'parse-error (funcall (parse-hex) (stream-from-str "0x100000000"))) "parse-hex: Invalid hexadecimal format or overflow")) ; Переполнение по длине строки
  
  (print "Тестирование hex parser с кастомными разделителями (с аргументами)...")
  (let ((custom-delims (list #\g #\G)))
    (let ((parser (parse-hex custom-delims)))
      (print (assertcar (funcall parser (stream-from-str "0xFFg")) 255))
      (print (assertcar (funcall parser (stream-from-str "0x10G")) 16))
      (print (assertcar (funcall parser (stream-from-str "0xABC")) 2748))
      (print (assert (catch 'parse-error (funcall parser (stream-from-str "0x123 "))) "parse-hex: Number not followed by a delimiter or end-of-stream")))))


(run-tests)