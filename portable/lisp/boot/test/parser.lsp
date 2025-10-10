(unit-tests 'parser)

(deftest parse-elem-test ()
  "Тесты для parse-elem"
  (let ((parse-4-digit (parse-elem 4))
	(parse-1-digit (parse-elem 1)))
    (print (assert (funcall parse-4-digit '(4 5 6)) '((4 5 6))))
    (print (assert (funcall parse-4-digit '(1 2 3)) nil))
    (print (assert (funcall parse-1-digit '(4 5 6)) nil))
    (print (assert (funcall parse-1-digit '(1 2 3)) '((1 2 3))))))

(deftest parse-and-test ()
  "Тесты для &&&"
  (print (assert (funcall (&&& (parse-elem 'A) (parse-elem 'B)) '(A B C D)) '(((A B) . (C D)))))
  (print (assert (funcall (&&& (parse-elem 'A) (parse-elem 'B)) '(A C B)) nil))
  (print (assert (funcall (&&& (parse-elem 'A) (parse-elem 'B)) '(B C B)) nil))
  (print (assert (funcall (&&& (parse-elem 'B) (parse-elem 'C)) '(B C A)) '(((B C) . (A)))))
  (print (assert (funcall (&&& (parse-elem 'B) (parse-elem 'C)) '(B C A A A D)) '(((B C) . (A A A D)))))
  (print (assert (funcall (&&& (parse-elem 'A) (parse-elem 'B) (parse-elem 'C)) '(A B C D)) '(((A B C) . (D))))))

(deftest parse-app-test ()
  "Тесты для parse-app"
  (print (assert (funcall (parse-app (&&& (parse-elem 'A) (parse-elem 'B)) #'car) '(A B C D)) '((A . (C D)))))
  (print (assert (funcall (parse-app (parse-or (parse-suc '(1)) (parse-suc '(2))) #'car) '(A B)) '((1 . (A B)) (2 . (A B))))))

(deftest parse-or-test ()
  "Тесты для parse-or"
  (print (assert (funcall (parse-or (parse-elem 'A) (parse-elem 'B) (parse-elem 'C)) '(A B C)) '((a . (b c)))))
  (print (assert (funcall (parse-or (parse-elem 'C) (parse-elem 'B) (parse-elem 'D)) '(B C D)) '((b . (c d)))))
  (print (assert (funcall (parse-or (parse-elem 'C) (parse-elem 'B) (parse-elem '1)) '(1 C)) '((1 . (c)))))
  (print (assert (funcall (parse-or (parse-elem 'A) (parse-elem 'B)) '(C B B)) nil)))

(deftest parse-many-test ()
  "Тесты для parse-many"
  (print (assert (funcall (parse-many (parse-elem 'A)) '(A A A A B C A)) '(((a a a a) . (b c a)))))
  (print (assert (funcall (parse-many (parse-or (parse-elem 'A) (parse-elem 'B))) '(A A b A B C A)) '(((a a b a b) . (c a)))))
  (print (assert (funcall (parse-many (parse-elem 'B)) '(A C B)) '((() . (a c b))))))

(deftest parse-some-test ()
  "Тесты для parse-some"
  (print (assert (funcall (parse-some (parse-elem 'A)) '(A A A A B C A)) '(((a a a a) . (b c a)))))
  (print (assert (funcall (parse-some (parse-or (parse-elem 'A) (parse-elem 'B))) '(A A b A B C A)) '(((a a b a b) . (c a)))))
  (print (assert (funcall (parse-some (parse-elem 'B)) '(A C B)) nil)))

(deftest parse-pred-test ()
  "Тесты для parse-pred"
  (print (assert (funcall (parse-pred #'is-digit) '(#\1 B C)) '((#\1 . (b c)))))
  (print (assert (funcall (parse-pred #'is-alpha) '(#\a 1 B C)) '((#\a . (1 b c))))))

(deftest parse-many-sep-test ()
  "Тесты для parse-many-sep"
  (print "parse-many-sep")
  (let ((parser (parse-many-sep (parse-elem 1) (parse-elem 0))))
    (print (assert (funcall parser '(1 0 1 0 1 2)) '(((1 1 1) . (2)))))
    (print (assert (funcall parser '(2 1 0 1)) nil))
    (print (assert (funcall parser '()) '((() . ()))))))

(deftest parse-some-sep-test ()
  "Тесты для parse-some-sep"
  (print "parse-some-sep")
  (let ((parser (parse-some-sep (parse-elem 1) (parse-elem 0))))
    (print (assert (funcall parser '(1 0 1 2)) '(((1 1) . (2)))))
    (print (assert (funcall parser '(2 1 0 1)) nil))
    (print (assert (funcall parser '()) nil))
    (print (assert (funcall parser '(1)) '(((1) . ()))))))

(run-tests)