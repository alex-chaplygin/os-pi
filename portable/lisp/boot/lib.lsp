(defun null (x)
  (eq x (quote())))

(defun append (list1 list2)
  "объединение двух списков (1 . (2 . nil)) (a . (b . nil))"
  "(append '(1 2) '(a b))"
  "(1 . (append (2) '(a b)))"
  "(1 2 . (a b))"
  "(1 2 a b)"
  (if (null (cdr list1))
      (cons (car list1) list2)
    (cons (car list1) (append (cdr list1) list2))))

(defun fac(x)
  (cond
    ((= x 1) 1)
    (t (* x (fac (- x 1))))))

(defun caar(x) (car (car x)))
(defun cadar(x) (car (cdr (car x))))
(defun cdar(x) (cdr (car x)))

(defun get-bit (num bit)
  "Получение бита с номером bit у числа num"
  (& (>> num bit) 1))

(defmacro if (test true false)
  `(cond (,test ,true)
	 (t ,false)))

(defmacro inner-for (name var start end body)
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
  `((lambda ,(get-vars vars) ,body)
    ,@(get-vals vars)))

(defun get-vars (v)
  (if (null v) nil
      (cons (caar v) (get-vars (cdr v)))))

(defun get-vals (v)
  (if (null v) nil
      (cons (cadar v) (get-vals (cdr v)))))

(defmacro ++ (x)
  `(+ ,x 1))
