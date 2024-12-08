; модульный тест для библиотечных функций
(defun for-test ()
  "Тест для проверки захвата переменной c, которая есть в цикле и в параметре функции"
  (labels ((test (c) c))
    (for c 0 10
	 (print (test "10")))))

(defun fors-test ()
  "Тест вложенных циклов"
  (for i 0 3
       (for j 0 2
	    (for k 0 3
		 (for l 0 4
		      (for m 0 3
			   (print `(,i ,j ,k ,l ,m))))))))

(defun tagbody-rec-test ()
  "Тест рекурсивного вызова при последовательном чтении массива для JPEG"
  "Вместо while цикла надо использовать unless"
  (let ((ar #(10 20 30 10 20 30))
	(i 0))
    (labels ((read-num ()
	       (let ((a (aref ar i)))
		 (setq i (++ i))
		 a))
	     (read ()
	       (let* ((a (read-num))
		      (val (case (progn (print `(while a ,a)) a)
				     (10 (progn (print "10") t))
				     (20 (progn (print "20") t))
				     (otherwise nil))))
		 (print `(begin read))
		 (print `(a = ,a val = ,val))
		 (unless (null val) (read)))))
      (read)
      (print (read-num)))))

(defun lex-test ()
  "Тест лексической переменной, где параметр list у функции map закрывает свободную переменную list в let"
  "В динамическом Лиспе - результат (0 0 0), в лексическом - правильный"
  (let ((list '(a b c)))
    (print (assert (map #'(lambda (x) (nth list x)) '(2 1 0)) '(c b a)))))

(defun compose-test ()
  "Тест композиции функций"
  (labels ((o (f g) #'(lambda (x) (funcall f (funcall g x))))
	   (sqr (x) (* x x)))
	  (print (assert (funcall (o #'sqr #'sqr) 10) 10000))))

(defun dyn-test ()
  "Тест динамических переменных"
  (defvar *g* 10)
  (labels ((test (x) (+ x *g*)))
	  (print (assert (test 1) 11))
	  (setq *g* 20)
	  (print (assert (test 1) 21))))

(defun closure-test ()
  "Тест лексических замыканий"
  (labels ((make (am) (let ((bal am))
			#'(lambda (act)
			    (case act
				  ('inc (incf bal))
				  ('bal bal)
				  ('dec (decf bal))
				  (otherwise nil)))))
	   (pr (acc a) (print `(acc bal = ,(funcall a 'bal)))))
	  (let ((a (make 10))
		(b (make 20)))
	    (print "Init")
	    (pr 'a a)
	    (funcall a 'inc)
	    (print "Inc")
	    (pr 'a a)
	    (funcall a 'dec)
	    (print "Dec")
	    (pr 'a a)
	    (print "Init")
	    (pr 'b b)
	    (funcall b 'inc)
	    (print "Inc")
	    (pr 'b b)
	    (funcall b 'dec)
	    (print "Dec")
	    (pr 'b b)
	    (print "a = ")
	    (pr 'a a))))

(defun cond-tests ()
  (labels ((cond-test (val)
	     (cond
	       ((> val 10) ">10")
	       ((< val 10) "<10")
	       (t "==10"))))
    (print (assert (cond-test 3) "<10"))
    (print (assert (cond-test 7) "<10"))
    (print (assert (cond-test 10) "==10"))
    (print (assert (cond-test 12) ">10"))
    (print (assert (cond) nil))))

(defun and-tests ()
  (print (assert (and nil nil) nil))
  (print (assert (and (= 1 1) nil) nil))
  (print (assert (and nil t) nil))
  (print (assert (and t t) t))
  (print (assert (and t t nil) nil))
  (print (assert (and t nil t t) nil))
  (print (assert (and t t t) t))
  (print (assert (and nil nil nil) nil)))

(defun or-tests ()
  (print (assert (or nil nil) nil))
  (print (assert (or nil t) t))
  (print (assert (or t nil) t))
  (print (assert (or t t) t))
  (print (assert (or (= 1 1) nil nil) t))
  (print (assert (or nil nil nil) nil)))

(fors-test)
(tagbody-rec-test)
(lex-test)
(compose-test)
(dyn-test)
(closure-test)
(cond-tests)
(and-tests)
(or-tests)
