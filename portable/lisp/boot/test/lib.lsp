; модульный тест для библиотечных функций
(unit-tests 'lib)

(deftest let-test ()
  "Тесты для макроса let (метод черного ящика)."

  (print "Простое присваивание")
  (print (assert (let ((x 1) (y 2)) (+ x y)) 3))

  ;; (print "Вложенные let")
  ;; (print (assert (let ((x 1)) 
  ;;             (let ((y 2)) (+ x y))) 3))

  ;; (print "Пустое тело let")
  ;; (print (assert (let ((x 1) (y 2)) nil) nil))

  ;; (print "Множественные вычисления в теле let")
  ;; (print (assert (let ((x 1) (y 2))
  ;;             (setq x (+ x y))
  ;;             (setq y (* x y))
  ;;             (+ x y)) 9))

  ;; (print "Лексическая область видимости")
  ;; (print (assert (let ((x 1))
  ;; 	    (let ((x 2)) x)) 2))
  
  ;; (print "let без переменных")
  ;; (print (assert (let () 42) 42))

  ;; ;; Неправильные случаи для самого let
  ;; (print "Неправильные случаи для let")

  ;; ;; Пустой let без привязок и тела
  ;; (progn (let ()) nil)

  ;; ;; Отсутствие тела после списка привязок
  ;; (let ((x 1)))

  ;; ;; Привязка без переменной
  ;; (let ((() 1)) nil)

  ;; ;; Привязка без значения
  ;; (progn (let ((x)) nil))

  ;; ;; Привязка с лишними элементами
  ;; (progn (let ((x 1 2)) nil))

  ;; ;; Некорректный список привязок (не список)
  ;; (progn (let (x 1) nil)))
  )

(deftest when-test ()
  "Тест функции when"
  (print (assert (when t 78) 78))     
  (print (assert (when nil 78) nil)))

(deftest unless-test ()
  "Тест для проверки функции unless"
  (print (assert (unless t 42) nil))
  (print (assert (unless nil 42) 42)))

(deftest for-test ()
  "Тест для проверки захвата переменной c, которая есть в цикле и в параметре функции"
  (labels ((test (c) c))
    (for c 0 10
	 (print (test "10")))))

(deftest fors-test ()
  "Тест вложенных циклов"
  (for i 0 3
       (for j 0 2
	    (for k 0 3
		 (for l 0 4
		      (for m 0 3
			   (print `(,i ,j ,k ,l ,m))))))))

(deftest while-test ()
  "Тесты для проверки работы макроса while"
  ;; Простой тест
  (let ((x 0))
    (while (< x 5)
      (setq x (++ x)))
    (print (assert x 5)))
  ;; Тест с нулевым количеством итераций
  (let ((x 0))
    (while nil
      (setq x (++ x)))
    (print (assert x 0)))
  ;; Тест вложенных циклов
  (let ((x 0) (y 0))
    (while (< x 3)
      (setq y 0)
      (while (< y 2)
        (print `(x ,x y ,y))
        (setq y (++ y)))
      (setq x (++ x)))))

(deftest tagbody-rec-test ()
  "Тест рекурсивного вызова при последовательном чтении массива для JPEG
  Вместо while цикла надо использовать unless"
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

(deftest lex-test ()
  "Тест лексической переменной, где параметр list у функции map закрывает свободную переменную list в let
  В динамическом Лиспе - результат (0 0 0), в лексическом - правильный"
  (let ((list '(a b c)))
    (print (assert (map #'(lambda (x) (nth list x)) '(2 1 0)) '(c b a)))))

(deftest compose-test ()
  "Тест композиции функций"
  (labels ((sqr (x) (* x x)))
	  (print (assert (funcall (o #'sqr #'sqr) 10) 10000))))

(deftest dyn-test ()
  "Тест динамических переменных"
  (defvar *g* 10)
  (labels ((test (x) (+ x *g*)))
	  (print (assert (test 1) 11))
	  (setq *g* 20)
	  (print (assert (test 1) 21))))

(deftest closure-test ()
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

(deftest cond-tests ()
  "Тесты cond"
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

(deftest and-tests ()
  "Тесты and"
  (print (assert (and nil nil) nil))
  (print (assert (and (= 1 1) nil) nil))
  (print (assert (and nil t) nil))
  (print (assert (and t t) t))
  (print (assert (and t t nil) nil))
  (print (assert (and t nil t t) nil))
  (print (assert (and t t t) t))
  (print (assert (and nil nil nil) nil)))

(deftest or-tests ()
  "Тесты or"  
  (print (assert (or nil nil) nil))
  (print (assert (or nil t) t))
  (print (assert (or t nil) t))
  (print (assert (or t t) t))
  (print (assert (or (= 1 1) nil nil) t))
  (print (assert (or nil nil nil) nil)))

(deftest case-test ()
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
  ;; (case 3
  ;;   ())
  )

(deftest get-bit-test ()
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

(deftest null-test ()
  "Тесты функции null."
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

(deftest not-test ()
  "Тесты функции not"
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

(deftest inc-test ()
  "Тест функции ++"
  (let ((tests '((5 6)             
                 (-3 -2)           
                 (0 1))))            
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (++ input) expected))))))

(deftest dec-test ()
  "Тест функции --"
  (let ((tests '((5 4)           
                 (-3 -4)         
                 (0 -1))))         
    (dolist (test tests)
      (let ((input (car test))
            (expected (cadr test)))
        (print (assert (-- input) expected))))))

(deftest incf-test ()
  "Тест функции incf (с использованием ++)."
  (let ((tests '((5 6) 
                 (-3 -2) 
                 (0 1))))
    (dolist (test tests)
      (let ((x (car test)))
        (incf x)
        (print (assert (= x (second test)) t))))))

(deftest decf-test ()
  "Тест функции decf (с использованием --)."
  (let ((tests '((5 4) 
                 (-3 -4) 
                 (0 -1))))
    (dolist (test tests)
      (let ((x (car test)))
        (decf x)
        (print (assert (= x (second test)) t))))))

(deftest setf-test ()
  "Тест для проверки работы макроса setf"
  ;; (let ((x 10)
  ;;       (hash (make-hash)))
  ;;   ;; Тест для проверки установки значения переменной
  ;;   (setf x 20)
  ;;   (print (assert (= x 20) t))
  ;;   ;; Тест для проверки установки значения в хеш-таблице
  ;;   (setf (slot hash 'key) 42)
  ;;   (print (assert (= (get-hash hash 'key) 42) t))
  ;;   ;; Тест для проверки ошибки при передаче некорректного аргумента
  ;;   (print (assert (equal (setf '(1 2 3) 5) "setf: invalid var") t)))
  )

(deftest defvar-test ()
  "Тест для проверки создания переменной без значения (должна быть инициализирована nil)"
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

(deftest swap16-test ()
  "Тест для проверки swap16"
  (print (assert (swap16 0xabcd) 0xcdab))
  (print (assert (swap16 0x1) 0x0100)))

(deftest handle-test ()
  "Тест для проверки handle"
  (print (assert (handle 1
			 (a (k) k)) 1))
  (print (assert (handle (progn (raise 'a 2) 3)
			 (a (k) k)) 2))
  (print (assert (handle (progn (raise 'b 2) 5)
  			 (a (k) (+ k 1))
  			 (b (k) (- k 2))) 0)))

(run-tests)
