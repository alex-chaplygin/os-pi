; функции для работы со списками

(defun caar(x) (car (car x)))
(defun caadr(x) (car (car (cdr x))))
(defun cadr(x) (car (cdr x)))
(defun second(x) (car (cdr x)))
(defun caddr(x) (car (cdr (cdr x))))
(defun third(x) (car (cdr (cdr x))))
(defun cadddr(x) (car (cdr (cdr (cdr x)))))
(defun forth(x) (car (cdr (cdr (cdr x)))))
(defun caddddr(x) (car (cdr (cdr (cdr (cdr x))))))
(defun fifth(x) (car (cdr (cdr (cdr (cdr x))))))
(defun cadar(x) (car (cdr (car x))))
(defun cdar(x) (cdr (car x)))
(defun cdadr(x) (cdr (car (cdr x))))
(defun cddr(x) (cdr (cdr x)))
(defun cdddr(x) (cdr (cdr (cdr x))))
(defun cddddr(x) (cdr (cdr (cdr (cdr x)))))

(defun list (&rest args)
  "Функция создания списка"
  args)

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

(defun o (f g) #'(lambda (x) (funcall f (funcall g x)))) ; математическая композиция функция f и g

(defun app (f list)
  "Применяет функцию f к каждому элементу списка list"
  "(app '(lambda (x) (set-hash h x nil)) '(x y z))"
  (if (null list) nil
      (progn
	(funcall f (car list))
	(app f (cdr list)))))

(defmacro dolist (params &rest bod)
  "Вариант app, обходит список с итерационной переменной"
  "(dolist (x list) (setq a x) (setq b x))"
  (let ((loops (gensym))
	(tests (gensym))
	(var (car params))
	(list (gensym)))
    `(tagbody
	(setq ,list ,(second params))
	(go ,tests)
	,loops
	(setq ,var (car ,list))
	(setq ,list (cdr ,list))
	,@bod
	,tests
	(if (null ,list) nil (go ,loops)))))
	
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
    (funcall f (car list) (foldr f start (cdr list)))))

(defun last (lst)
  "Найти последний элемент списка"
  (if (null lst) (error "last: empty list")
      (if (null (cdr lst)) (car lst) (last (cdr lst)))))

(defun filter (pred list)
  "Остаются только те элементы списка list, для которых предикат pred с одним параметров возвращает t"
  (if (null list) nil
      (let ((h (car list))
	    (tail (filter pred (cdr list))))
	(if (funcall pred h) (cons h tail) tail))))

(defun sort (pr list)
  "Быстрая сортировка списка list по предикату pr (2 параметра)"
  (cond ((null list) nil)
	((null (cdr list)) list)
	(t (let ((head (car list))
		 (tail (cdr list)))
	     (append (sort pr (filter #'(lambda (x) (funcall pr x head)) tail))
		     (cons head (sort pr (filter #'(lambda (x) (not (funcall pr x head))) tail))))))))

(defun minp* (pred min list)
  (if (null list) min
    (let ((first (car list)))
      (minp* pred
	     (if (funcall pred min first) min first) (cdr list)))))

(defun minp (pred list)
  "Возвращает минимальный элемент списка по предикату. Предикат pred принимает два элемента списка и возвращает T, если первый элемент меньше второго"
  (minp* pred (car list) (cdr list)))

(defun contains (list elem)
  "Предикат - элемент elem содержится в списке list"
  (let ((result NIL))
    (dolist (current list)
      (when (= current elem)
	(setq result T)))
    result))

(defun reverse (lst)
  "Переворачивает список"
  (foldl #'(lambda (acc elem) (cons elem acc)) nil lst))

(defun remove-dupl(list)
  "Удаляет повторяющиеся элементы из списка"
  (let ((unique NIL))
    (dolist (elem list)
      (when (not (contains unique elem))
	(setq unique (cons elem unique)))) ; элементы добавляются в начало списка
    (reverse unique))) ; переворачиваем список обратно

(defun list-length (list)
  "длина списка list"
  (if (null list)
      0
      (++ (list-length (cdr list))))) 

(defun list-search (list element)
  "Находит индекс элемента element в списке list."
  (labels ((search (list index)
	     (if (null list) nil ; Возвращаем nil, если элемент не найден
		 (if (equal element (car list)) index ; Возвращаем индекс при совпадении
		     (search (cdr list) (++ index))))))
    (search list 0)))

(defun nth (list n)
  "Получает n-й элемент списка list"
  (if (null list) (error "nth: invalid index")
    (if (= n 0) (car list)
      (nth (cdr list) (- n 1)))))

(defun list-to-array (list)
  "Преобразование списка в массив"
  (let* ((n (list-length list))
	 (arr (make-array n)))
    (for i 0 n
	 (seta arr i (car list))
	 (setq list (cdr list)))
    arr))
