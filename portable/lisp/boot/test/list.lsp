					; тестирование функций для списков
(assert (minp '(4 1 5 2) '<) 1)
(assert (filter '(4 2 5 1) '(lambda (x) (< x 3))) '(2 1))
(assert (sort '(4 1 5 2) '<) '(1 2 4 5))
