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

(defun case-test ()
  "Тест для проверки макроса case"
  ;; Тест для проверки случая, когда значение совпадает с одним из вариантов
  (print (assert (case 1
                   (1 2)
                   (otherwise 3)) 2))
  ;; Тест для проверки случая, когда значение не совпадает ни с одним из вариантов
  (print (assert (case 10
                   (1 2)
                   (otherwise 3)) 3))
  ;; Тест для проверки случая с несколькими вариантами
  (print (assert (case 2
                   (1 2)
                   (2 3)
                   (otherwise 4)) 3))
  ;; Тест для проверки случая с otherwise
  (print (assert (case 5
                   (1 2)
                   (2 3)
                   (otherwise 4)) 4))
  ;; Тест для проверки случая с пустым списком вариантов
  (print (assert (case 5
                   (otherwise 4)) 4))
  ;; Тест для проверки случая с несколькими otherwise (должен использоваться первый)
  (print (assert (case 5
                   (otherwise 4)
                   (otherwise 5)) 4))
  ;; Тест для проверки случая с вложенными case
  (print (assert (case 1
                   (1 (case 2
                        (2 3)
                        (otherwise 4)))
                   (otherwise 5)) 3))
  ;; Тест для проверки случая с пустым списком вариантов и без otherwise
  (case 3
    ()))

(defun get-bit-test ()
  "Тест для проверки функции get-bit"
  ; Тесты для числа 5 (двоичное представление: 101)
  (print (assert (get-bit 5 0) 1)) 
  (print (assert (get-bit 5 1) 0)) 
  (print (assert (get-bit 5 2) 1))
  (print (assert (get-bit 5 -1) 0)) 
  ; Тест для числа 0 (двоичное представление: 0)
  (print (assert (get-bit 0 0) 0)) 
  ; Тесты для отрицательных чисел 
  (print (assert (get-bit -1 0) 1))
  (print (assert (get-bit -1 31) 1))
  ; Граничный тест 
  (print (assert (get-bit 1024 10) 1))
  (print (assert (get-bit 1024 11) 0))
  (print (assert (get-bit 2147483647 0) 1)))

(defun null-test ()
  "Тест функции null."
  (print "Тест функции null")
  (let ((tests '((() t)            ; Пустой список
                 ((1 2 3) ())      ; Непустой список
                 (a ())            ; Символ
                 (nil ())          ; nil
                 ((nil) ())        ; Список с nil
                 ((()) ()))))      ; Список с пустым списком
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (null input) expected))))))

(defun not-test ()
  "Тест функции not"
  (print "Тест функции not.")
  (let ((tests '((t ())             ; t -> nil
                 (() t)            ; nil -> t
                 (() t)             ; Пустой список -> t
                 ((1) ())           ; Непустой список -> nil
                 (a ())             ; Символ -> nil
                 (42 ())            ; Число -> nil
                 ("string" ()))))   ; Строка -> nil
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (not input) expected))))))

(defun inc-test ()
  "Тест функции ++"
  (print "Тест функции ++.")
  (let ((tests '((5 6)             
                 (-3 -2)           
                 (0 1))))            
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (++ input) expected))))))

(defun dec-test ()
  "Тест функции --"
  (print "Тест функции --.")
  (let ((tests '((5 4)           
                 (-3 -4)         
                 (0 -1))))         
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (-- input) expected))))))

(defun setf-test ()
  (print "Тест для проверки работы макроса setf")
  (let ((x 10)
        (hash (make-hash)))
    ;; Тест для проверки установки значения переменной
    (setf x 20)
    (print (assert (= x 20) t))
    ;; Тест для проверки установки значения в хеш-таблице
    (setf (slot hash 'key) 42)
    (print (assert (= (get-hash hash 'key) 42) t))
    ;; Тест для проверки ошибки при передаче некорректного аргумента
    (print (assert (equal (setf '(1 2 3) 5) "setf: invalid var") t))))

(defun defvar-test ()
  (print "Тест для проверки работы макроса defvar")
  ;; Тест для проверки ссздания переменной без значения (должна быть инициализирована nil)
  (defvar *test-var1*)
  (print (assert (null *test-var1*) t))
  ;; Тест для проверки ссздания переменной со значением
  (defvar *test-var2* 42)
  (print (assert (= *test-var2* 42) t))
  ;; Тест для проверки ссздания переменной с nil в качестве значения
  (defvar *test-var3* nil)
  (print (assert (null *test-var3*) t))
  ;; Тест для проверки ссздания переменной с выражением в качестве значения
  (defvar *test-var4* (+ 10 20))
  (print (assert (= *test-var4* 30) t)))

(fors-test)
(tagbody-rec-test)
(lex-test)
(compose-test)
(dyn-test)
(closure-test)
(cond-tests)
(and-tests)
(or-tests)
(case-test)
(get-bit-test)
(null-test)
(not-test)
(inc-test)
(dec-test)
(setf-test)
(defvar-test)
