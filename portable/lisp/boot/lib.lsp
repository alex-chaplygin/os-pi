(defun null (x)
  "Проверка на пустое значение"
  (eq x ()))

(defun list (&rest args)
  "Функция создания списка"
  args)

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
(defun cadadr(x) (car (cdr (car (cdr x)))))
(defun cdar(x) (cdr (car x)))
(defun cdadr(x) (cdr (car (cdr x))))
(defun cddr(x) (cdr (cdr x)))
(defun cdddr(x) (cdr (cdr (cdr x))))
(defun cddddr(x) (cdr (cdr (cdr (cdr x)))))

(defmacro incf (var)
  "Увеличивает значение переменной на 1"
  `(setq ,var (++ ,var)))

(defmacro decf (var)
  "Увеличивает значение переменной на 1"
  `(setq ,var (-- ,var)))

(defun abs (x)
  "Абсолютное значение числа"
  (if (< x 0) (- 0 x) x))

(defun not(x)
  "Логическое отрицание"
  (if (eq x nil) t nil))

(defun o (f g) #'(lambda (x) (funcall f (funcall g x)))) ; математическая композиция функций f и g

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
     (if (= ,var ,end) 'end
	 (progn ,@body 
		     (,name (+ ,var 1)))))
  `(,name ,start))

(defmacro for (var start end &rest body)
  "Цикл for, переменная var от start до end - 1"
  "body - тело цикла"
  "(for i 0 10 (seta arr i i))"
  `(let ((,var ,start))
     (tagbody
	(while (< ,var ,end)
	       ,@body (setq ,var (+ ,var 1))))))

(defmacro while (test &rest bod)
  "Цикл while"
  (let ((loops (gensym))
	(tests (gensym)))
    `(tagbody
	(go ,tests)
	,loops
	,@bod
	,tests
	(when ,test (go ,loops))
      break)))

(defmacro until (test &rest bod)
  "Цикл until"
  (let ((loops (gensym))
	(tests (gensym)))
    `(tagbody
	,loops
	,@bod
	,tests
	(unless ,test (go ,loops))
	break)))

(defmacro break ()
  "Прерывание текущего цикла while, for, until"
  '(go break))

(defmacro let (vars &rest body)
  "Блок локальных переменных"
  "(let ((x 0)
         (y 0))
        (+ x y))"
  "((lambda (x y) 
       (+ x y)) 0 0)"
  `((lambda ,(get-vars vars) ,@body)
    ,@(get-vals vars)))

(defmacro let* (vars &rest body)
  "Создаёт блок из локальных переменных, которые могут использовать друг друга при задании начального значения"
  (if (null (cdr vars)) `(let ,vars ,@body)
      `(let (,(car vars)) (let* ,(cdr vars) ,@body))))

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

(defun clone (obj)
  "Делает копию объекта (кроме чисел, массивов и строк)"
  (cond ((null obj) nil)
	((atom obj) obj)
	(t (cons (clone (car obj)) (clone (cdr obj))))))

(defun swap16 (num)
  "Меняет старший и младший байт местами для 16 битного числа"
  (let ((high-byte (& (>> num 8) 0xff))
	(low-byte (& num 0xff)))
    (+ high-byte (<< low-byte 8))))

(defmacro raise (name args)
  "Вызов исключения с именем name и параметрами args"
  `(throw ,name ,args))

(defmacro *handle-r* (exit expr handlers)
  "Рекурсивный макрос для handle"
  (if (null handlers) expr
    (let ((handler (car handlers)))
      `((lambda ,(second handler) ,@(cddr handler))
	(catch ',(car handler) (throw ',exit (*handle-r* ,exit ,expr ,(cdr handlers))))))))

(defmacro handle (expr &rest handlers)
  "Обработка исключения для выражения"
  (let ((exit (gensym)))
    `(catch ',exit (*handle-r* ,exit ,expr ,handlers))))
