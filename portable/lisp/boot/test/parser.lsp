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

(deftest parse-and-macro-test ()
  "Тесты макроса parse-and"
  (let ((s1 (stream-from-str "abcd")))
    (print (assertcar (funcall (&&& (parse-elem #\a)  (parse-elem #\b)) s1) '(#\a #\b)))
    (print (assertcar (funcall (&&& a->(parse-elem #\a)
				    b->(parse-elem #\b)
				    return (cons a b)) s1) '(#\a . #\b)))
    ))

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
    (print (assertcar (funcall (parse-many (parse-elem #\1)) s1) '()))
    (print (assertcar (funcall (parse-many (parse-elem #\a)) (stream-from-str "aaaabca")) '(#\a #\a #\a #\a)))
    (print (assertcar (funcall (parse-many (parse-or (parse-elem #\a) (parse-elem #\b))) (stream-from-str "aababca")) '(#\a #\a #\b #\a #\b)))
  ))
  ;; (print (assert (funcall (parse-many (parse-elem 'B)) '(A C B)) '((() . (a c b))))))

(deftest parse-many-n-test ()
  "Тесты для parse-many-n"
  (let ((s1 (stream-from-str "aaab"))
	(s2 (stream-from-str "bba")))
    (print (assertcar (funcall (parse-many-n 3 (parse-elem #\a)) s1) '(#\a #\a #\a)))
    (print (assert (funcall (parse-many-n 4 (parse-elem #\a)) s1) '()))
    (print (assertcar (funcall (parse-many-n 2 (parse-elem #\a)) s1) '(#\a #\a)))
    (print (assert (funcall (parse-many-n 2 (parse-elem #\a)) s2) '()))
  ))

(deftest parse-some-test ()
  "Тесты для parse-some"
  (let ((s1 (stream-from-str "aac1"))
	(s2 (stream-from-str "baac1")))
    (print (assertcar (funcall (parse-some (parse-elem #\a)) s1) '(#\a #\a)))
    (print (assertcar (funcall (parse-some (parse-elem #\b)) s2) '(#\b)))
    (print (assert (funcall (parse-some (parse-elem #\c)) s2) nil))))

(deftest parse-pred-test ()
  "Тесты для parse-pred"
  (print (assertcar (funcall (parse-pred #'is-digit) (stream-from-str "1aa")) #\1))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "avc")) nil))
  (print (assert (funcall (parse-pred #'is-digit) (stream-from-str "")) nil)))

(deftest parse-optional-test ()
  "Тесты для комбинатора parse-optional (необязательный парсер)"
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "456")) #\4))
  (print (assertcar (funcall (parse-optional (parse-elem #\4)) (stream-from-str "123")) nil)))

(deftest parse-sep-test ()
  "Тесты для комбинатора с разделителем"
  (let ((parser (parse-sep (parse-elem 1) (parse-elem 0))))
    (print (assertcar (funcall parser (stream-from-arr #(1 0 1 0 1 2) t)) '(1 1 1)))
    (print (assertcar (funcall parser (stream-from-arr #(0 1 0 1 0 1 2) t)) '(1 1 1)))
    (print (assertcar (funcall parser (stream-from-arr #(2 0 1 0 1 2) t)) nil))))

(run-tests)
