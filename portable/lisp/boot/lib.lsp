(defun null (x)
  (eq x (quote())))

(defun fac(x)
  (cond
    ((= x 1) 1)
    (t (* x (fac (- x 1))))))

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

(defmacro for (var start end body)
  "Цикл for, переменная var от start до end - 1"
  "body - тело цикла"
  `(defun for-func (,var)
     (cond ((= ,var ,end) 'end)
	   (t (progn
		,body 
		(for-func (+ ,var 1))))))
  `(for-func ,start))
