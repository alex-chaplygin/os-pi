;; тестирование функций для списков
(unit-tests 'list)

(deftest append-tests ()
  "Тесты функций объединения списков"
  (print "Тесты функций объединения списков")
  (print (assert (append '(1 2) '(3 4)) '(1 2 3 4)))
  (print (assert (append '() '(1 2)) '(1 2)))
  (print (assert (append '(1 2) '()) '(1 2)))
  (print (assert (append) nil))
  (print (assert (append '(1)) '(1)))
  (print (assert (append '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6)))
  (print (assert (append '() '() '()) '())))

(deftest app-tests ()
  "Тесты функции app"
  (print "Тесты функции app")
  ;; Тест 1: Пустой список
  (print (assert (app #'(lambda (x) (print x)) nil) nil))
  ;; Тест 2: Список с одним элементом
  (print (assert (app #'(lambda (x) (print x)) '(a)) nil))
  ;; Тест 3: Список с несколькими элементами
  (print (assert (app #'(lambda (x) (print x)) '(1 2 3)) nil)))

(deftest list-length-tests ()
  "Тесты возвращения длины списка"
  (print "Тесты возвращения длины списка")
  (print (assert (list-length '(4 1 5 2)) 4))
  (print (assert (list-length '(a)) 1))
  (print (assert (list-length nil) 0))
  (print (assert (list-length '()) 0)))

(deftest do-list-test ()
  "Тест dolist"
  (dolist (a '(1 2 3 4 5))
    (print a))
  (dolist (a '())
    (print a))
  (dolist (a '(42))
    (print a))
  (dolist (a '(1 2))
    (dolist (y '(a b))
      (print (list a y)))))
  ;; (dolist (x 10)
  ;;   (print x)))
  ;; (dolist (x '(1 2 3) extra)
  ;;   (print x)))
;;  (dolist ())
  ;; (dolist (x)
  ;;   (print x)))
;;  (dolist (10 '(1 2 3))
;;    (print 1)))

(deftest map-tests ()
  "Тесты функции map"
  (print "Тесты функции map")
  (print (assert (map #'(lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16)))
  (print (assert (map #'(lambda (x) (+ x 1)) '()) nil))
;  (map "not-a-function" '(1 2 3))
					;(map #'(lambda (x) (* x x)) 42))
  )

(deftest foldl-tests ()
  "Тесты левоассоциативной свертки"
  (print "Тесты левоассоциативной свертки")
  (print (assert (foldl #'+ 0 '(1 2 3 4)) 10))
  (print (assert (foldl #'(lambda (acc x) (* acc x)) 1 '(1 2 3 4)) 24))
  (print (assert (foldl #'+ 0 '()) 0))
 ; (foldl "not-a-function" 0 '(1 2 3))
					;  (foldl #'+ 0 42))
  )

(deftest foldr-tests ()
  "Тесты правоассоциативной свертки"
  (print "Тесты правоассоциативной свертки")
  (print (assert (foldr #'+ 0 '(1 2 3 4)) 10))
  (print (assert (foldr #'(lambda (x acc) (* x acc)) 1 '(1 2 3 4)) 24))
  (print (assert (foldr #'+ 0 '()) 0))
  ;(foldr "not-a-function" 0 '(1 2 3))
					;(foldr #'+ 0 42))
  )

(deftest minp-tests ()
  "Тесты возврата минимального элемента списка по предикату"
  (print "Тесты возврата минимального элемента списка по предикату")
  (print (assert (minp #'< '(4 1 5 2)) 1))
  (print (assert (minp #'> '(4 3 1 2)) 4)))

(deftest filter-tests ()
  "Тесты фильтрации списка по предикату"
  (print "Тесты фильтрации списка по предикату")
  (print (assert (filter #'(lambda (x) (< x 3)) '(4 2 5 1)) '(2 1)))
  (labels ((pred (x) (>= x 4)))
    (print (assert (filter #'pred '(4 2 5 1)) '(4 5))))
  (print (assert (filter #'(lambda (x) t) '(1 2 3)) '(1 2 3)))
  (print (assert (filter #'(lambda (x) nil) '(1 2 3)) nil))
  ;;(filter 123 '(1 2 3))
  ;;(filter #'(lambda (x) (< x 3)) 123))
  )

(deftest sort-tests ()
  "Тесты сортировки списка"
  (print "Тесты сортировки списка")
  (print (assert (sort #'< '(4 1 5 2)) '(1 2 4 5)))
  (print (assert (sort #'> '(4 3 1 2)) '(4 3 2 1)))
  (print (assert (sort #'> '(10 -1 0 5)) '(10 5 0 -1)))
  (print (assert (sort #'< '(100)) '(100)))
  (print (assert (sort #'< '()) '()))
  ;; Тест на повторяющиеся элементы
  (print (assert (sort #'< '(3 1 2 1 3)) '(1 1 2 3 3)))
  (print (assert (sort #'> '(3 1 2 1 3)) '(3 3 2 1 1)))
  ;; Тест на уже отсортированный список
  (print (assert (sort #'< '(1 2 3 4 5)) '(1 2 3 4 5)))
  (print (assert (sort #'> '(5 4 3 2 1)) '(5 4 3 2 1)))
  ;; Тест на одинаковые элементы
  (print (assert (sort #'< '(7 7 7 7)) '(7 7 7 7)))
  (print (assert (sort #'> '(7 7 7 7)) '(7 7 7 7)))
  ;;(sort #'> 'b)
  ;;(sort 42 '(1 2 3)))
  )

(deftest list-search-tests ()
  "Тесты поиска элемента в списке"
  (print "Тесты поиска элемента в списке")
  (print (assert (list-search '(1 2 3) 1) 0))
  (print (assert (list-search '(1 2 3) 6) nil))
  (print (assert (list-search '(1 2 3) 3) 2))
  (print (assert (list-search '(1 2 3 1 2 3) 2) 1))
  (print (assert (list-search '() 3) nil))
  (print (assert (list-search '(1) 1) 0))
  (print (assert (list-search '(1) 2) nil))
  ;;(list-search 123 1))
  )

(deftest nth-tests ()
  "Тесты возврата n-го элемента списка"
  (print "Тесты возврата n-го элемента списка")
  (print (assert (nth '(1 2 3) 0) 1))
  (print (assert (nth '(1 2 3) 2) 3))
  ;;(nth '(1 2 3) 10)
  ;;(nth 42 0))
  )

(deftest list-tests ()
  "Тесты функции создания списка"
  (print "Тесты функции создания списка")
  (print (assert (list 1 2 3) '(1 2 3)))
  (print (assert (list '(1 2) 3 4 5) '((1 2) 3 4 5)))
  (print (assert (list "a" "b" "c") '("a" "b" "c"))))

(deftest list-get-tests ()
  "Тесты функций работы с вложенными списками"
  (print "Тесты функций работы с вложенными списками")
  (print (assert (caar '((1 2) (3 4))) 1))
  (print (assert (caadr '((1 2) (3 4))) 3))
  (print (assert (cadr '(1 2 3)) 2))
  (print (assert (caddr '(1 2 3 4)) 3))
  (print (assert (cadddr '(1 2 3 4 5)) 4))
  (print (assert (cadar '((1 2) (3 4))) 2))
  (print (assert (cdar '((1 2) (3 4))) '(2)))
  (print (assert (cdadr '((1 2) (3 4))) '(4)))
  (print (assert (cddr '(1 2 3)) '(3))))

; тесты для o (композиция функций)

(deftest last-tests ()
  "Тесты функции возвращения последнего элемента списка"
  (print "Тесты функции возвращения последнего элемента списка")
  (print (assert (last '(1 2 3 4)) 4))
  (print (assert (last '(a)) 'a))
  ;;  (last '()))
  )

(deftest contains-tests ()
  "Тесты функции проверки наличия элемента в списке"
  (print "Тесты функции проверки наличия элемента в списке")
  (print (assert (contains '(1 2 3) 2) t))
  (print (assert (contains '(1 2 3) 5) nil))
  (print (assert (contains '() 1) nil))
  (print (assert (contains '(1) 1) t))   ;; Список с одним элементом, элемент есть в списке
  (print (assert (contains '(1) 2) nil)) ;; Список с одним элементом, элемента нет  в списке
  (print (assert (contains '(1 2 2 3) 2) t)) ;; Элементы повторяются в списке
  ;;(print (assert (contains "hello" "h") nil)))
  )

(deftest zip-with-test ()
  (print "Тест zip-with")
  (print (assert (zip-with #'+ '(1 2) '(3 4)) '(4 6)))
  (print (assert (zip-with #'+ '() '()) '()))  
  (print (assert (zip-with #'+ '(42) '(1)) '(43)))
  ;;(zip-with #'+ '(1) ())
;;  (zip-with 42 '(1 2 3) '(4 5 6))
;;  (zip-with #'+ 42 '(1 2 3))
  ;;  (zip-with #'+ '(1 2 3) 42))
  )
  
(deftest reverse-tests ()
  "Тесты функции reverse"
  (print "Тесты функции reverse")
  (print (assert  (reverse '(1 2 3)) '(3 2 1))) ;3 числа
  (print (assert  (reverse '(1)) '(1))) ;1 число
  (print (assert  (reverse '()) '())) ;пустой текст
  (print (assert  (reverse '((1 2) (3 4))) '((3 4) (1 2))))
  ;;(reverse 42))
  )

(deftest remove-dupl-tests ()
  "Тесты функции удаления повторяющихся элементов"
  (print "Тесты функции удаления повторяющихся элементов")
  (print (assert (remove-dupl '(1 2 3 2 1 4 5 6 5 6 4)) '(1 2 3 4 5 6)))
  (print (assert (remove-dupl '("a" "b" "c" "d" "b" "d" "b" "c")) '("a" "b" "c" "d") ))
  (print (assert (remove-dupl '(1 1 1 1 1)) '(1)))
  (print (assert (remove-dupl '(1 2 3 4 5)) '(1 2 3 4 5)))
  (print (assert (remove-dupl '()) '()))
  (print (assert (remove-dupl '(24)) '(24)))
  ;;(remove-dupl 24))
  )

(deftest list-to-array-tests ()
  "Тесты ф-ции преобразования списка в массив"
  (print   "Тесты ф-ции преобразования списка в массив")
  (print (assert (list-to-array '(1 2 3)) #(1 2 3)))
  (print (assert (list-to-array '(a b c)) #(a b c)))
  (print (assert (list-to-array '()) #())) 
  (print (assert (list-to-array '("kakoj-to text")) #("kakoj-to text")))
  (print (assert (list-to-array '(nil)) #(nil)))
  ;;(list-to-array 42))
  )

(deftest make-list-tests ()
  "Тесты ф-ции создания списка"
  (print (assert (make-list 5) '(() () () () ())))
  (print (assert (make-list 3 1) '(1 1 1)))
  (print (assert (make-list 0) '())))

(deftest flatten-tests ()
  "Тесты ф-ции преобразования многоуровнего списка в одноуровневый"
  (print (assert (flatten '((1 2) (3 4) (5 6))) '(1 2 3 4 5 6)))
  (print (assert (flatten '((1 2 (3 4)) (5 6))) '(1 2 3 4 5 6))))

(run-tests)
