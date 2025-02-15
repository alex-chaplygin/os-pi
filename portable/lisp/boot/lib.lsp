(defun null (x)
  "Проверка на пустое значение"
  (eq x ()))

(defun fac(x)
  (cond
    ((< x 2) 1)
    (t (* x (fac (- x 1))))))

(defun not(x)
  "Логическое отрицание"
  (if (eq x nil) t nil))

(defun get-bit (num bit)
  "Получение бита с номером bit у числа num"
  (& (>> num bit) 1))

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

(defmacro cond (&rest body)
  "Условный оператор cond"
  (if (null body)
      nil
      (let ((c (car body)))
	(if (cdr c)
	    `(if ,(car c)
		 (progn ,@(cdr c))
		 (cond ,@(cdr body)))
	    (car c)))))

(defmacro and (&rest args)
  "оператор 'и' для произвольного количества выражений"
  (if (null args) t
      (if (null (cdr args)) (car args)
	  `(if ,(car args) (and ,@(cdr args)) nil))))

(defmacro or (&rest args)
  "оператор 'или' для произвольного количества выражений" 
  (if (null args) nil
      (if (null (cdr args))
          (car args)
          `(if ,(car args) t (or ,@(cdr args))))))


(defmacro case (val &rest list)
  "(setq k 10)"
  "(case k ((1 2)(2 3)(otherwise 4)))"
  "(cond ((equal k 1) 2)
         ((equal k 2) 3)
         (t 4))"
  `(cond ,@(map #'(lambda (x) (case-func x val)) list)))

(defun case-func (p val)
  "(e v) -> ((equal val e) v)"
  "(otherwise v) -> (t v)"
  (when (or (not (pairp p)) (null p))
    (error "case: invalid condition"))
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
;`(inner-for ,(intern (concat "for-" (symbol-name var))) ,var ,start ,end ,@body))
 `(let ((,var ,start))
     (while (< ,var ,end)
	    ,@body (setq ,var (+ ,var 1)))))

(defmacro while (test &rest bod)
  "Цикл while"
  (let ((loops (gensym))
	(tests (gensym)))
    `(tagbody
	(go ,tests)
	,loops
	,@bod
	,tests
	(if ,test (go ,loops) nil))))

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

(defmacro != (g1 g2)
  "Не равно"
  `(not (equal ,g1 ,g2)))

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

(setq *gensym-counter* 0) ;счетчик уникальных символов

(defun gensym ()
  "Возвращает уникальный символ типа G<n>, где n - новое число"
  (intern (concat "G" (inttostr (incf *gensym-counter*)))))

(defun clone (obj)
  "Делает копию объекта (кроме чисел, массивов и строк)"
  (cond ((null obj) nil)
	((atom obj) obj)
	(t (cons (clone (car obj)) (clone (cdr obj))))))

(defmacro incf (var)
  "Увеличивает значение переменной на 1"
  `(setq ,var (++ ,var)))

(defmacro decf (var)
  "Увеличивает значение переменной на 1"
  `(setq ,var (-- ,var)))
