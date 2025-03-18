; функции для работы со списками

(defun append2 (list1 list2)
  "объединение двух списков (1 . (2 . nil)) (a . (b . nil))"
  "(append '(1 2) '(a b))"
  "(1 . (append (2) '(a b)))"
  "(1 2 . (a b))"
  "(1 2 a b)"
  (if (null list1) list2
    (if (null (cdr list1))
	(cons (car list1) list2)
      (cons (car list1) (append2 (cdr list1) list2)))))

(defmacro append (&rest lists)
  "Объединение произвольного количества списков. Если нет аргументов, возвращает nil."
  (if (null lists) nil
      `(append2 ,(car lists) (append ,@(cdr lists)))))
      
(defun app (f list)
  "Применяет функцию f к каждому элементу списка list"
  "(app '(lambda (x) (set-hash h x nil)) '(x y z))"
  (if (null list) nil
      (progn
	(funcall f (car list))
	(app f (cdr list)))))

(defun list-length (list)
  "длина списка list"
  (if (null list)
      0
      (++ (list-length (cdr list))))) 

(defmacro dolist (params &rest bod)
  "Вариант app, обходит список с итерационной переменной"
  "(dolist (x list) (setq a x) (setq b x))"
  (if (not (and (pairp params)
   		(= (list-length params) 2)
   		(symbolp (car params))
		))
		     (error "dolist: incorrect arguments")
  (let ((loops (gensym))
	(tests (gensym))
	(var (car params))
	(list (gensym)))
    `(if (not (pairp ,(second params)))
	 (error "dolist: incorrect list")
    (tagbody
	(setq ,list ,(second params))
	(go ,tests)
	,loops
	(setq ,var (car ,list))
	(setq ,list (cdr ,list))
	,@bod
	,tests
	(if (null ,list) nil (go ,loops)))))))
	
(defun map (f list)
  "Применяет функцию f к каждому элементу списка list и возвращает новый список"
  (when (not (and (pairp list) (functionp f)))
      (error "map: incorrect arguments"))
  (if (null list) nil
    (cons (funcall f (car list)) (map f (cdr list)))))

(defun foldl (f start list)
  "Левоассоциативная свертка (foldl):"
  "(f ... (f (f start elem_1) elem_2) ... elem_n)"
  (when (not (and (pairp list) (functionp f)))
      (error "foldl: incorrect arguments"))
  (labels ((foldl* (lst acc)
	     (if (null lst) acc
		 (foldl* (cdr lst) (funcall f acc (car lst))))))
    (foldl* list start)))

(defun foldr (f start list)
  "Правоассоциативная свертка (foldr):"
  "(f elem_1 (f elem_2 ... (f elem_n start) ... ))"
  (when (not (and (pairp list) (functionp f)))
    (error "foldr: incorrect arguments"))
  (labels ((foldr* (start list)
	     (if (null list) start
		 (funcall f (car list) (foldr* start (cdr list))))))
    (foldr* start list)))

(defun last (lst)
  "Найти последний элемент списка"
  (cond ((not (pairp lst)) (error "last: not list"))
	((null lst) (error "last: empty list"))
	((null (cdr lst)) (car lst))
	(t (last (cdr lst)))))

(defun filter (pred list)
  "Остаются только те элементы списка list, для которых предикат pred с одним параметров возвращает t"
  (unless (functionp pred)
    (error "filter: not function in argument"))
  (unless (pairp list)
    (error "filter: incorrect list"))
  (labels ((filter* (list)
	     (if (null list) nil
		 (let ((h (car list))
		       (tail (filter* (cdr list))))
		   (if (funcall pred h) (cons h tail) tail)))))
    (filter* list)))

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
  (when (not (pairp list))
      (error "remove-dupl: incorrect arguments"))
  (let ((unique NIL))
    (dolist (elem list)
      (when (not (contains unique elem))
	(setq unique (cons elem unique)))) ; элементы добавляются в начало списка
    (reverse unique))) ; переворачиваем список обратно

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

(defun zip-with (f list1 list2)
  "Соединить два списка list1 и list2 с помощью функции от 2-х аргументов f"
  "Списки должны иметь одинаковую длину"
  (when (!= (list-length list1) (list-length list2))
    (error "zip-with: lists have different length"))
  (labels ((z (l1 l2)
	     (if (null l1) nil
		 (cons (funcall f (car l1) (car l2)) (z (cdr l1) (cdr l2))))))
    (z list1 list2)))
