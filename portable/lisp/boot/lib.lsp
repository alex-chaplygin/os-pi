(defun null (x)
  (eq x (quote())))

(defun fac(x)
  (cond
    ((= x 1) 1)
    (t (* x (fac (- x 1))))))

(defun caar(x) (car (car x)))
(defun cadar(x) (car (cdr (car x))))

(defun get-bit (num bit)
  "Получение бита с номером bit у числа num"
  (& (>> num bit) 1))

(defmacro if (test true false)
  `(cond (,test ,true)
	 (t ,false)))

(defmacro for (var start end body)
  "Цикл for, переменная var от start до end - 1"
  "body - тело цикла"
  `(defun for-func (,var)
     (cond ((= ,var ,end) 'end)
	   (t (progn
		,body 
		(for-func (+ ,var 1))))))
  `(for-func ,start))

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
