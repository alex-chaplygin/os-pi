;; Тест компиляции программы.
(defun test-compile (expr expected-res)
  (let ((program (compile expr)))
    (print "Expression")
    (print expr)
    (print "Compiler")
    (print program)
    (print "Generator")
    (dolist (ins (generate program))
      (print ins))
    (print (assert program expected-res))))

(test-compile '(progn 1 2 3) '(seq (const 1) (seq (const 2) (const 3))))
(test-compile '(progn) '(const ()))
(test-compile '(if 1 (progn 1 3) (progn 2))
	      '(ALTER (CONST 1) (SEQ (CONST 1) (CONST 3)) (CONST 2)))
(test-compile '(if t t nil)
	      '(ALTER (GLOBAL-REF 0) (GLOBAL-REF 0) (GLOBAL-REF 1)))
(test-compile '(progn (setq a 1 b 2) a b)
	      '(SEQ (SEQ (GLOBAL-SET 2 (CONST 1)) (GLOBAL-SET 3 (CONST 2))) (SEQ (GLOBAL-REF 2) (GLOBAL-REF 3))))

(test-compile '(setq a (+ (* 1 2) (* 2 3)))
	      '(GLOBAL-SET 2 (PRIM + ((PRIM * ((CONST 1) (CONST 2))) (PRIM * ((CONST 2) (CONST 3)))))))

(test-compile '(defun test (x) (setq a 2) (setq x a) x)
	      '(LABEL TEST (SEQ (SEQ (GLOBAL-SET 2 (CONST 2)) (SEQ (LOCAL-SET 0 (GLOBAL-REF 2)) (LOCAL-REF 0))) (RETURN))))

(test-compile '(progn (defun test (x) x x) (test 10))
	      '(SEQ (LABEL TEST (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 0)) (RETURN))) (REG-CALL TEST 0 ((CONST 10)))))

(test-compile '(progn (defun test (x y) (progn x y)) (test 10 (if t 3 4)))
	      '(SEQ (LABEL TEST (SEQ (SEQ (LOCAL-REF 0) (LOCAL-REF 1)) (RETURN))) (REG-CALL TEST 0 ((CONST 10) (ALTER (GLOBAL-REF 0) (CONST 3) (CONST 4))))))

(test-compile '((lambda (x) ((lambda (y) (cons x y)) 1)) 2)
	      '(FIX-LET 1 ((CONST 2)) (FIX-LET 1 ((CONST 1)) (PRIM CONS ((DEEP-REF 1 0) (LOCAL-REF 0))))))

(test-compile '(progn (defun fac (x) (if (equal x 1) 1 (* x (fac (- x 1))))) (fac 4))
	      '(SEQ (LABEL FAC (SEQ (ALTER (PRIM EQUAL ((LOCAL-REF 0) (CONST 1))) (CONST 1) (PRIM * ((LOCAL-REF 0) (REG-CALL FAC 0 ((PRIM - ((LOCAL-REF 0) (CONST 1)))))))) (RETURN))) (REG-CALL FAC 0 ((CONST 4)))))

(test-compile '(setq a #'(lambda (x y) (+ x y)))
	      '(GLOBAL-SET 2 (FIX-CLOSURE G598 (LABEL G598 (SEQ (PRIM + ((LOCAL-REF 0) (LOCAL-REF 1))) (RETURN))))))

(test-compile '(progn (defun test () 1) (setq a #'test))
	      '(SEQ (LABEL TEST (SEQ (CONST 1) (RETURN))) (GLOBAL-SET 2 (FIX-CLOSURE TEST ()))))

(test-compile '(defun test () #'(lambda (x) x))
	      '(LABEL TEST (SEQ (FIX-CLOSURE G711 (LABEL G711 (SEQ (LOCAL-REF 0) (RETURN)))) (RETURN))))

(test-compile '(progn (defmacro test (x y) `(+ ,x ,y)) (test 1 2))
	      '(SEQ (NOP) (PRIM + ((CONST 1) (CONST 2)))))
