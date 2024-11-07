					; тестирование функций для списков
(assert (minp '(4 1 5 2) '<) 1)
(assert (filter '(lambda (x) (< x 3)) '(4 2 5 1)) '(2 1))
(labels ((pred (x) (>= x 4)))
  (assert (filter 'pred '(4 2 5 1)) '(4 5)))
(assert (sort '(4 1 5 2) '<) '(1 2 4 5))
(assert (list-length '(4 1 5 2)) 4)
(assert (list-length '(a)) 1)
(assert (list-length nil) 0)
