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

(test-compile '(defun test (x) (setq a 2) (setq x a) x)
	      '(LABEL TEST (FIX-CLOSURE 1 (SEQ (GLOBAL-SET 2 (CONST 2)) (SEQ (LOCAL-SET 0 (GLOBAL-REF 2)) (LOCAL-REF 0))))))

(test-compile '(progn (defun test (x) x x) (test 10))
	      '(SEQ (LABEL TEST (FIX-CLOSURE 1 (SEQ (LOCAL-REF 0) (LOCAL-REF 0)))) (SEQ (ALLOC 1) (REG-CALL TEST ((CONST 10))))))

(test-compile '(progn (defun test (x y) (progn x y)) (test 10 (if t 3 4)))
	      '(SEQ (LABEL TEST (FIX-CLOSURE 2 (SEQ (LOCAL-REF 0) (LOCAL-REF 1)))) (SEQ (ALLOC 2) (REG-CALL TEST ((CONST 10) (ALTER (GLOBAL-REF 0) (CONST 3) (CONST 4)))))))

(test-compile '((lambda (x) ((lambda (y) (cons x y)) 1)) 2)
	      '(SEQ (LABEL G274 (FIX-CLOSURE 1 (SEQ (LABEL G275 (FIX-CLOSURE 1 (PRIM CONS ((DEEP-REF 1 0) (LOCAL-REF 0))))) (ALLOC 1) (REG-CALL G275 ((CONST 1)))))) (ALLOC 1) (REG-CALL G274 ((CONST 2)))))

(test-compile '(progn (defun fac (x) (if (equal x 1) 1 (* x (fac (- x 1))))) (fac 4))
	      '(SEQ (LABEL FAC (FIX-CLOSURE 1 (ALTER (PRIM EQUAL ((LOCAL-REF 0) (CONST 1))) (CONST 1) (PRIM * ((LOCAL-REF 0) (SEQ (ALLOC 1) (REG-CALL FAC ((PRIM - ((LOCAL-REF 0) (CONST 1))))))))))) (SEQ (ALLOC 1) (REG-CALL FAC ((CONST 4))))))
