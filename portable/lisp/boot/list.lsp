; функции для работы со списками
(defun append (list1 list2)
  "объединение двух списков (1 . (2 . nil)) (a . (b . nil))"
  "(append '(1 2) '(a b))"
  "(1 . (append (2) '(a b)))"
  "(1 2 . (a b))"
  "(1 2 a b)"
  (if (null list1) list2
    (if (null (cdr list1))
	(cons (car list1) list2)
      (cons (car list1) (append (cdr list1) list2)))))

(defun app (f list)
  "Применяет функцию f к каждому элементу списка list"
  "(app '(lambda (x) (set-hash h x nil)) '(x y z))"
  (if (null list) nil
    (progn
      (funcall f (car list))
      (app f (cdr list)))))

(defun map (f list)
  "Применяет функцию f к каждому элементу списка list и возвращает новый список"
  (if (null list) nil
    (cons (funcall f (car list)) (map f (cdr list)))))

(defun foldl (f start list)
  "Левоассоциативная свертка (foldl):"
  "(f ... (f (f start elem_1) elem_2) ... elem_n)"
  (defun foldl*(list a)
    (if (null list) a
      (foldl* (cdr list) (funcall f a (car list)))))
  (foldl* list start))

(defun foldr (f start list)
  "Правоассоциативная свертка (foldr):"
  "(f elem_1 (f elem_2 ... (f elem_n start) ... ))"
  (if (null list) start
    (funcall f (foldr f start (cdr list)) (car list))))

(defun last (lst)
  "Найти последний элемент списка"
  (if (null lst) '(error "last: empty list")
      (if (null (cdr lst)) (car lst) (last (cdr lst)))))
