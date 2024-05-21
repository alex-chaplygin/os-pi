(defun null (x)
  "Проверка на пустое значение"
  (eq x (quote())))

(defun fac(x)
  (cond
    ((< x 2) 1)
    (t (* x (fac (- x 1))))))

(defun not(x)
  "Логическое отрицание"
  (if (eq x t) nil t))

(defun caar(x) (car (car x)))
(defun caadr(x) (car (car (cdr x))))
(defun cadr(x) (car (cdr x)))
(defun caddr(x) (car (cdr (cdr x))))
(defun cadar(x) (car (cdr (car x))))
(defun cdar(x) (cdr (car x)))
(defun cddr(x) (cdr (cdr x)))

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

(defmacro unless (test &rest body)
  "Условный неполный оператор"
  "test - условие"
  "body - список выражений по лжи"
  `(if ,test nil (progn ,@body)))

(defmacro when (test &rest body)
  "Условный неполный оператор"
  "test - условие"
  "body - список выражений по истине"
  `(if ,test (progn ,@body) nil))

(defmacro case (val &rest list)
  "(setq k 10)"
  "(case k ((1 2)(2 3)(otherwise 4)))"
  "(cond ((equal k 1) 2)
         ((equal k 2) 3)
         (t 4))"
  `(cond ,@(map '(lambda (x) (case-func x val)) list)))

(defun case-func (p val)
  "(e v) -> ((equal val e) v)"
  "(otherwise v) -> (t v)"
  (if (eq (car p) 'otherwise)
      (list t (cadr p))
      (list (list 'equal (car p) val) (cadr p))))

(defmacro inner-for (name var start end &rest body)
  "Вспомогательная функция для for"
  `(defun ,name (,var)
     (cond ((= ,var ,end) 'end)
	   (t (progn ,@body 
		     (,name (+ ,var 1))))))
  `(,name ,start))

(defmacro for (var start end &rest body)
  "Цикл for, переменная var от start до end - 1"
  "body - тело цикла"
  "(for i 0 10 (seta arr i i))"
  `(inner-for ,(intern (concat "for-" (symbol-name var))) ,var ,start ,end ,@body))

(defmacro while (test &rest body)
  "Цикл while"
  `(cond ((not ,test) 'end)
    (t (progn ,@body
        (while ,test ,@body)))))

(defmacro let (vars &rest body)
  "Блок локальных переменных"
  "(let ((x 0)
         (y 0))
        (+ x y))"
  "((lambda (x y) 
       (+ x y)) 0 0)"
  `((lambda ,(get-vars vars) ,@body)
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

(defmacro inner-let* (vars vals &rest body)
  "Вспомогательная функция для let"
  `((lambda (,(car vars)) (if ,(null (cdr vars))
			       (progn ,@body)
			       (inner-let* ,(cdr vars) ,(cdr vals) ,@body)))
			     ,(car vals)))

(defmacro let* (vars &rest body)
  "Создаёт блок из локальных переменных, которые могут использовать друг друга при задании начального значения"
  `(inner-let* ,(get-vars vars) ,(get-vals vars) ,@body))


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

(defmacro -- (x)
  "Декремент значения"
  `(- ,x 1))

(defmacro >= (g1 g2)
  "Больше или равно"
  `(or (> ,g1 ,g2) (equal ,g1 ,g2)))

(defmacro = (g1 g2)
  "Равно"
  `(equal ,g1 ,g2))

(defmacro <= (g1 g2)
  "Меньше или равно"
  `(or (< ,g1 ,g2) (equal ,g1 ,g2)))

(defmacro setf (var val)
  "Если var - символ, то устанавливает его значение"
  "Если var - (slot obj key), то устанавливает значение по ключу"
  (cond ((atom var) `(setq ,var ,val))
	((eq (car var) 'slot) `(set-hash ,(cadr var) ,(caddr var) ,val))
	(t "setf: invalid var")))

(defmacro defvar (name &rest value)
  "Создаёт новый глобальный символ. (defvar имя значение)"
  (if (null value)
      `(setf ,name nil)
      (if (not (null (cdr value))) ''error
	  `(setf ,name ,(car value))))) 

(defmacro defconst (name val)
  "Создать константу"
  `(defvar ,name ,val))

(defun putstring (s)
  "Печать строки"
  (for i 0 (string-size s) 
       (putchar (char s i))))

(defun abs (x)
  "Абсолютное значение"
  (if (< x 0) (- 0 x) x))

(defun expt (x y)
  "Возведение в степень"
  (if (equal y 0) 1
    (progn
      (for xx 1 y
        (setq x (* x x)))
      x)))

(defvar *gensym-counter* 0) ;счетчик уникальных символов

(defun gensym ()
  "Возвращает уникальный символ типа G<n>, где n - новое число"
  (setf *gensym-counter* (++ *gensym-counter*))
  (intern (concat "G" (inttostr *gensym-counter*))))

(defun clone (obj)
  "Делает копию объекта (кроме чисел, массивов и строк)"
  (cond ((null obj) nil)
	((atom obj) obj)
	(t (cons (clone (car obj)) (clone (cdr obj))))))
