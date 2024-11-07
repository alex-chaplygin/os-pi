					; тестирование функций для списков
(assert (minp '(4 1 5 2) '<) 1)
(assert (filter '(4 2 5 1) '(lambda (x) (< x 3))) '(2 1))
(labels ((pred (x) (>= x 4)))
  (assert (filter '(4 2 5 1) 'pred) '(4 5)))
(assert (sort '(4 1 5 2) '<) '(1 2 4 5))
(assert (list-length '(4 1 5 2)) 4)
(assert (list-length '(a)) 1)
(assert (list-length nil) 0)
