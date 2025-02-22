(defvar *test-compile-failed*)

;; Тест компиляции программы.
(defun test-compile (expr expected-res)
  (print (block compiler
    (print "Expression")
    (print expr)
    (print "Compiler")
    (let ((program (compile expr)))
      (print program)
      (when (pairp program)
	(print "Generator")
	(dolist (ins (generate program))
	  (print ins))
	(return-from 'compiler (assert program expected-res)))))))

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
	      '(SEQ (LABEL TEST (SEQ (FIX-PRIM CONS ((LOCAL-REF 0) (LOCAL-REF 1))) (RETURN))) (SEQ (NARY-CALL TEST 1 0 ((CONST 10))) (SEQ (NARY-CALL TEST 1 0 ((CONST 10) (CONST 20))) (NARY-CALL TEST 1 0 ((CONST 10) (CONST 20) (CONST 30)))))))

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
