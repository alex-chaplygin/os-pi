(defun null (x)
  "Проверка на пустое значение"
  (eq x (quote())))

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


(defun fac(x)
  (cond
    ((= x 1) 1)
    (t (* x (fac (- x 1))))))

(defun not(x)
  "Логическое отрицание"
  (if (eq x t) nil t))

(defun caar(x) (car (car x)))
(defun cadr(x) (car (cdr x)))
(defun caddr(x) (car (cdr (cdr x))))
(defun cadar(x) (car (cdr (car x))))
(defun cdar(x) (cdr (car x)))

(defun get-bit (num bit)
  "Получение бита с номером bit у числа num"
  (& (>> num bit) 1))

(defmacro if (test true false)
  "Условный оператор"
  "test - условие"
  "true - выражение по истине"
  "false - выражение по лжи"
  `(cond (,test ,true)
	 (t ,false)))

(defmacro inner-for (name var start end body)
  "Вспомогательная функция для for"
  `(defun ,name (,var)
     (cond ((= ,var ,end) 'end)
	   (t (progn ,body 
		     (,name (+ ,var 1))))))
  `(,name ,start))

(defmacro for (var start end body)
  "Цикл for, переменная var от start до end - 1"
  "body - тело цикла"
  `(inner-for ,(intern (concat "for-" (symbol-name var))) ,var ,start ,end ,body))

(defmacro let (vars body)
  "Блок локальных переменных"
  "(let ((x 0)
         (y 0))
        (+ x y))"
  "((lambda (x y) 
       (+ x y)) 0 0)"
  `((lambda ,(get-vars vars) ,body)
    ,@(get-vals vars)))

"(let* ((x 0)
        (y (+ x 1))
        (z (* y 5)))
     (+ x y))"
"((lambda (x)
    ((lambda (y)
      ((lambda (z)
         (+ x y z)) 
       (* y 5))
     (+ x 1))) 
   0)"

"(inner-let* (x y z) (0 (+ x 1) (*y 5)) (+ x y z))"

(defmacro inner-let* (vars vals body)
  "Вспомогательная функция для let"
  `((lambda (,(car vars)) ,(if (null (cdr vars))
			       body
			       `(inner-let* ,(cdr vars) ,(cdr vals) ,body)))
			     ,(car vals)))

(defmacro let* (vars body)
  "Создаёт блок из локальных переменных, которые могут использовать друг друга при задании начального значения"
  `(inner-let* ,(get-vars vars) ,(get-vals vars) ,body))


(defun get-vars (v)
  "Получение списка переменных для let"
  (if (null v) nil
      (cons (caar v) (get-vars (cdr v)))))

(defun get-vals (v)
  "Получение списка значений для let"
  (if (null v) nil
      (cons (cadar v) (get-vals (cdr v)))))

(defmacro ++ (x)
  "Инкремент значения"
  `(+ ,x 1))

(defmacro setf (var val)
  "Если var - символ, то устанавливает его значение"
  "Если var - (slot obj key), то устанавливает значение по ключу"
  (cond ((atom var) `(setq ,var ,val))
	((eq (car var) 'slot) `(set-hash ,(cadr var) ,(caddr var) ,val))
	(t "setf: invalid var")))
