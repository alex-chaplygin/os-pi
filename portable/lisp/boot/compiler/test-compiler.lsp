;; (defvar *test-compile-failed*)

;; Тест компиляции программы.
(defun test-compile (expr expected-res)
  (print (catch 'compiler
    (print "Expression")
    (print expr)
    (print "Compiler")
    (let ((program (compile expr)))
      (print program)
      (when (pairp program)
	(print "Generator")
	(dolist (ins (generate program))
	  (print ins))
	(throw 'compiler (assert program expected-res)))))))

(print "последовательность")
(test-compile '(progn 1 2 3) '(seq (const 1) (seq (const 2) (const 3))))
(print "пустой progn")
(test-compile '(progn) '(const ()))
(print "условие")
(test-compile '(if 1 (progn 1 3) (progn 2))
	      '(ALTER (CONST 1) (SEQ (CONST 1) (CONST 3)) (CONST 2)))
(test-compile '(if t t nil)
	      '(ALTER (GLOBAL-REF 0) (GLOBAL-REF 0) (GLOBAL-REF 1)))
(print "пустое условие")
(test-compile '(if) '())
(print "неверное число аргументов")
(test-compile '(if 1) '())
(test-compile '(if 1 2) '())
(test-compile '(if 1 2 3 4) '())
(print "присваивание")
(test-compile '(progn (setq a 1 b 2) a b)
	      '(SEQ (SEQ (GLOBAL-SET 2 (CONST 1)) (GLOBAL-SET 3 (CONST 2))) (SEQ (GLOBAL-REF 2) (GLOBAL-REF 3))))

(test-compile '(setq a (+ (* 1 2) (* 2 3)))
	      '(GLOBAL-SET 2 (FIX-PRIM + ((FIX-PRIM * ((CONST 1) (CONST 2))) (FIX-PRIM * ((CONST 2) (CONST 3)))))))
(print "пустое присваивание")
(test-compile '(setq) '())
(print "неверное число аргументов")
(test-compile '(setq a) '())
(test-compile '(setq a 1 b) '())
(print "не переменная")
(test-compile '(setq 1) '())
(test-compile '(setq a 1 2) '())

(print "примитив с фиксированным числом аргументов")
(test-compile '(eq 'a 'a) '(FIX-PRIM EQ ((CONST A) (CONST A))))
(print "неверное число аргументов")
(test-compile '(eq) '())
(test-compile '(eq 1) '())
(test-compile '(eq 1 2 2) '())

(print "примитив с переменным числом аргументов")
(test-compile '(+) '(NARY-PRIM + 0 ()))
(test-compile '(+ 1) '(NARY-PRIM + 0 ((CONST 1))))
(test-compile '(+ 1 2) '(NARY-PRIM + 0 ((CONST 1) (CONST 2))))
(test-compile '(+ 1 2 6 87 9 10 20) '(NARY-PRIM + 0 ((CONST 1) (CONST 2) (CONST 6) (CONST 87) (CONST 9) (CONST 10) (CONST 20))))
(print "неверное число аргументов")
(test-compile '(-) '())

(print "объявление функции")
(test-compile '(defun test (x) (setq a 2) (setq x a) x)
	      '(LABEL TEST (SEQ (SEQ (GLOBAL-SET 2 (CONST 2)) (SEQ (LOCAL-SET 0 (GLOBAL-REF 2)) (LOCAL-REF 0))) (RETURN))))
(print "пустое объявление функции")
;(test-compile '(defun) '())
(print "неправильные параметры")
;(test-compile '(defun ()) '())
;(test-compile '(defun test t) '())

(print "форма labels")
(test-compile '(labels ((test (x) (+ x x))
			(test2 (y) (* y y)))
		(test2 (test 10)))
	      '(SEQ (SEQ (LABEL G387 (SEQ (NARY-PRIM + 0 ((LOCAL-REF 0) (LOCAL-REF 0))) (RETURN))) (LABEL G388 (SEQ (NARY-PRIM * 0 ((LOCAL-REF 0) (LOCAL-REF 0))) (RETURN)))) (FIX-CALL G388 0 ((FIX-CALL G387 0 ((CONST 10)))))))

(print "пустая функция")
(test-compile '(defun test ()) '(LABEL TEST (SEQ (CONST ()) (RETURN))))

(print "вызов функции")
(test-compile '(progn (defun test (x) x x) (test 10))
	      '(SEQ (LABEL TEST (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 0)) (RETURN))) (FIX-CALL TEST 0 ((CONST 10)))))

(print "функция с переменным числом аргументов")
(test-compile '(progn (defun test (x &rest list) (cons x list)) (test 10) (test 10 20) (test 10 20 30))
	      '(SEQ (LABEL TEST (SEQ (FIX-PRIM CONS ((LOCAL-REF 0) (LOCAL-REF 1))) (RETURN))) (NARY-CALL TEST 1 0 ((CONST 10))) (NARY-CALL TEST 1 0 ((CONST 10) (CONST 20))) (NARY-CALL TEST 1 0 ((CONST 10) (CONST 20) (CONST 30))))))

(print "функция когда только переменное число аргументов")
(test-compile '(progn (defun rest-test (&rest list) list) (test 1 2 3))
	      '(SEQ (LABEL REST-TEST (SEQ (LOCAL-REF 0) (RETURN))) (NARY-CALL TEST 1 0 ((CONST 1) (CONST 2) (CONST 3)))))

(print "неверное число аргументов")
(test-compile '(progn (defun test (x &rest list) (cons x list)) (test)) '())

(test-compile '(progn (defun test (x y) (progn x y)) (test 10 (if t 3 4)))
	      '(SEQ (LABEL TEST (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 1)) (RETURN))) (FIX-CALL TEST 0 ((CONST 10) (ALTER (GLOBAL-REF 0) (CONST 3) (CONST 4))))))
(print "lambda выражение на месте функции")
(test-compile '((lambda (x) ((lambda (y) (cons x y)) 1)) 2)
	      '(FIX-LET 1 ((CONST 2)) (FIX-LET 1 ((CONST 1)) (FIX-PRIM CONS ((DEEP-REF 1 0) (LOCAL-REF 0))))))

(test-compile '(progn (defun fac (x) (if (equal x 1) 1 (* x (fac (- x 1))))) (fac 4))
	      '(SEQ (LABEL FAC (SEQ (ALTER (FIX-PRIM EQUAL ((LOCAL-REF 0) (CONST 1))) (CONST 1) (FIX-PRIM * ((LOCAL-REF 0) (FIX-CALL FAC 0 ((FIX-PRIM - ((LOCAL-REF 0) (CONST 1)))))))) (RETURN))) (FIX-CALL FAC 0 ((CONST 4)))))

(test-compile '(setq a #'(lambda (x y) (+ x y)))
	      '(GLOBAL-SET 2 (FIX-CLOSURE G598 (LABEL G598 (SEQ (FIX-PRIM + ((LOCAL-REF 0) (LOCAL-REF 1))) (RETURN))))))

(test-compile '(progn (defun test () 1) (setq a #'test))
	      '(SEQ (LABEL TEST (SEQ (CONST 1) (RETURN))) (GLOBAL-SET 2 (FIX-CLOSURE TEST ()))))

(test-compile '(defun test () #'(lambda (x) x))
	      '(LABEL TEST (SEQ (FIX-CLOSURE G844 (LABEL G844 (SEQ (LOCAL-REF 0) (RETURN)))) (RETURN))))
(print "Тест macro")
(test-compile '(progn (defmacro test (x y) `(+ ,x ,y)) (test 1 2))
	      '(SEQ (NOP) (NARY-PRIM + 0 ((CONST 1) (CONST 2)))))

(print "Тест comma-at")
(test-compile '(progn (defmacro test2 (&rest y) `(+ ,@y)) (test2) (test2 1) (test2 1 2) (test2 1 2 3))
	      '(SEQ (NOP) (SEQ (NARY-PRIM + 0 ()) (SEQ (NARY-PRIM + 0 ((CONST 1))) (SEQ (NARY-PRIM + 0 ((CONST 1) (CONST 2))) (NARY-PRIM + 0 ((CONST 1) (CONST 2) (CONST 3))))))))

;; (test-compile '(progn (setq a 1 b 2) `(a (,a) b (,b)))
;; 	      '(SEQ (...) (FIX-PRIM LIST ((CONST A) (FIX-PRIM LIST (GLOBAL-REF 2)) (CONST B) (FIX-PRIM LIST (GLOBAL-REF 3))))))

(print "Тест macro if неверное число аргументов")
(test-compile '(progn (defmacro test-a1 () (if 1)) (test-a1)) '())
(test-compile '(progn (defmacro test-a2 () (if 1 2)) (test-a2)) '())
(test-compile '(progn (defmacro test-a4 () (if 1 2 3 4)) (test-a4)) '())
(print "Тест macro if true")
(test-compile '(progn (defmacro test-if-t(&rest body) (if body `(car ',body) `(progn 1))) (test-if-t 2 3))
	      '(SEQ (NOP) (FIX-PRIM CAR ((CONST (2 3))))))
(print "Тест macro if false")
(test-compile '(progn (defmacro test-if-f(&rest body) (if body `(car ',body) `(progn 1))) (test-if-f))
	      '(SEQ (NOP) (CONST 1)))
(print "Тест macro primitive")
(test-compile '(progn (defmacro test-p() `(progn ,(+ 1 2))) (test-p))
	      '(SEQ (NOP) (CONST 3)))

(test-compile '(progn (defmacro test-p2() `(progn ,(setq a 2) ,(+ a 2))) (test-p2))
	      '(SEQ (NOP) (SEQ (CONST 2) (CONST 4))))

(print "Тест tagbody")
(test-compile '(tagbody
		(setq i 0)
		(setq a 0)
		(go test)
		loop
		(setq i (+ i 1))
		(setq a (+ a 10))
		test
		(if (equal i 10) nil (go loop)))
	      '(SEQ (GLOBAL-SET 2 (CONST 0)) (SEQ (GLOBAL-SET 3 (CONST 0)) (SEQ (GOTO TEST) (SEQ (LABEL LOOP) (SEQ (GLOBAL-SET 2 (NARY-PRIM + 0 ((GLOBAL-REF 2) (CONST 1)))) (SEQ (GLOBAL-SET 3 (NARY-PRIM + 0 ((GLOBAL-REF 3) (CONST 10)))) (SEQ (LABEL TEST) (SEQ (ALTER (FIX-PRIM EQUAL ((GLOBAL-REF 2) (CONST 10))) (GLOBAL-REF 1) (GOTO LOOP)) (CONST NIL))))))))))

(test-compile '(progn (setq a 1 b 2) `(a (,a) b (,b)))
	      '(SEQ
		(SEQ (GLOBAL-SET 2 (CONST 1)) (GLOBAL-SET 3 (CONST 2)))
		(FIX-PRIM CONS
		 ((CONST A)
		  (FIX-PRIM CONS
		   ((FIX-PRIM CONS
		     ((GLOBAL-REF 2) (CONST ())))
		    (FIX-PRIM CONS
		     ((CONST B)
		      (FIX-PRIM CONS
				((FIX-PRIM CONS ((GLOBAL-REF 3) (CONST ()))) (CONST ())))))))))))

(print "Тест cond в макросе")
(test-compile '(progn (defmacro test-cond (a)
			(cond ((eq a ()) 1)
			      (t 2))) (test-cond ()) (test-cond (1 2)))
	      '(SEQ (NOP) (SEQ (CONST 1) (CONST 2))))
(test-compile '(progn (defmacro test-cond (a) (cond)) (test-cond ()))
	      '(SEQ (NOP) (CONST ())))
(test-compile '(progn (defmacro test-cond (a) (cond ())) (test-cond ())) '())
(test-compile '(progn (defmacro test-cond (a) (cond (1))) (test-cond ())) '())
(test-compile '(progn (defmacro test-cond (a) (cond (1 2 3))) (test-cond ())) '())

(test-compile '(progn
(defun list (&rest args)
  "Функция создания списка"
  args)

(defun case-func (p val)
  (if (eq (car p) 'otherwise)
      (list t (car (cdr p)))
      (list (list 'equal (car p) val) (car (cdr p)))))

(defmacro cond (&rest body)
  "Условный оператор cond"
  (if (eq body ())
      nil
      (let ((c (car body)))
	(if (cdr c)
	    `(if ,(car c)
		 (progn ,@(cdr c))
		 (cond ,@(cdr body)))
	    (car c)))))

(defun map (f list)
  (if (eq list ()) nil
    (cons (funcall f (car list)) (map f (cdr list)))))

(defmacro case (val &rest list)
  `(cond ,@(map #'(lambda (x) (case-func x val)) list)))

(case 1 (1 2) (3 4) (otherwise 5)))
'(SEQ (LABEL LIST (SEQ (SEQ (CONST "Функция создания списка") (LOCAL-REF 1)) (RETURN))) (SEQ (LABEL CASE-FUNC (SEQ (ALTER (FIX-PRIM EQ ((FIX-PRIM CAR ((LOCAL-REF 0))) (CONST OTHERWISE))) (NARY-CALL LIST 0 0 ((GLOBAL-REF 0) (FIX-PRIM CAR ((FIX-PRIM CDR ((LOCAL-REF 0))))))) (NARY-CALL LIST 0 0 ((NARY-CALL LIST 0 0 ((CONST EQUAL) (FIX-PRIM CAR ((LOCAL-REF 0))) (LOCAL-REF 1))) (FIX-PRIM CAR ((FIX-PRIM CDR ((LOCAL-REF 0)))))))) (RETURN))) (SEQ (NOP) (SEQ (LABEL MAP (SEQ (ALTER (FIX-PRIM EQ ((LOCAL-REF 1) (CONST ()))) (GLOBAL-REF 1) (FIX-PRIM CONS ((NARY-PRIM FUNCALL 1 ((LOCAL-REF 0) (FIX-PRIM CAR ((LOCAL-REF 1))))) (FIX-CALL MAP 0 ((LOCAL-REF 0) (FIX-PRIM CDR ((LOCAL-REF 1)))))))) (RETURN))) (SEQ (NOP) (SEQ (CONST "Условный оператор cond") (ALTER (FIX-PRIM EQUAL ((CONST 1) (CONST 1))) (CONST 2) (SEQ (CONST "Условный оператор cond") (ALTER (FIX-PRIM EQUAL ((CONST 3) (CONST 1))) (CONST 4) (SEQ (CONST "Условный оператор cond") (ALTER (GLOBAL-REF 0) (CONST 5) (SEQ (CONST "Условный оператор cond") (CONST ()))))))))))))))

;; тест CATCH/THROW
(test-compile '(progn
		(defun throw-test (x)
		  (throw 'test x))
		(catch 'test
		  (throw-test 10)))
	      '(SEQ (LABEL THROW-TEST (SEQ (THROW (CONST TEST) (LOCAL-REF 0)) (RETURN))) (CATCH (CONST TEST) (FIX-CALL THROW-TEST 0 ((CONST 10))))))

;; тест append
(test-compile '(progn
		(defun null (x)
		  "Проверка на пустое значение"
		  (eq x ()))
		(defun append2 (list1 list2)
		  "объединение двух списков (1 . (2 . nil)) (a . (b . nil))"
		  "(append '(1 2) '(a b))"
		  "(1 . (append (2) '(a b)))"
		  "(1 2 . (a b))"
		  "(1 2 a b)"
		  (if (null list1) list2
		      (if (null (cdr list1))
			  (cons (car list1) list2)
			  (cons (car list1) (append2 (cdr list1) list2)))))
		(defmacro append (&rest lists)
		  "Объединение произвольного количества списков. Если нет аргументов, возвращает nil."
		  (if (null lists) nil
		      `(append2 ,(car lists) (append ,@(cdr lists)))))

		(append '(1 2) '(3 4))
		(append '(1 2))
		(append '(1 2) '())) '())

;; тест макрос внутри макроса с двойным квазицитированием
(test-compile '(progn
		(defmacro gen/e (name)
		  `(defmacro ,name (&rest params)
		     `((lambda (n)
			(+ ,@(cdr params))) 1)))
		(gen/e my-macro)
		(my-macro 1 2 3 4)) '(SEQ (NOP) (NOP) (NARY-PRIM + 0 ((CONST 1) (CONST 2) (CONST 3) (CONST 4)))))
		;; (defmacro gen/elem (name)
		;;   "Генерация макроса для класса name"
		;;   "Макрос создает объект с заданным списком свойств"
		;;   `(defmacro ,name (&rest params)
		;;      (let ((n ',name))
		;;        `(let ((new-elem (make-instance ,n)))
		;; 	  (set-defaults new-elem)
		;; 	  ,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
		;; 	  new-elem))))
		;; (gen/elem element)
		;; (setq e (element (x 4) (y 4) (width 2) (height 2)))

;; тест глобальная переменная внутри макроса (print `(screen ,screen))

;; (print 'testing)
;; (print
;;  (vm-run
;;   (assemble
;;    (generate
;;     (SEQ (GLOBAL-SET 2 (CONST 1))
;; 	  (SEQ (GLOBAL-SET 3 (CONST 2))
;; 	       (FIX-PRIM CONS ((CONST A)
;; 			   (FIX-PRIM CONS ((FIX-PRIM CONS ((GLOBAL-REF 2)
;; 						   (CONST ())))
;; 				       (FIX-PRIM CONS ((CONST B)
;; 						   (FIX-PRIM CONS ((FIX-PRIM CONS ((GLOBAL-REF 3)
;; 									   (CONST ())))
;; 							       (CONST ())))))))))))))))
;; (print 'done)
