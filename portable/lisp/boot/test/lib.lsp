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
  "Тест, где параметр list у функции map закрывает свободную переменную list в let"
  "В динамическом Лиспе - результат (0 0 0), в лексическом - правильный"
  (let ((list '(a b c)))
    (print (assert (map '(lambda (x) (nth list x)) '(2 1 0)) '(c b a)))))

(fors-test)
(tagbody-rec-test)
