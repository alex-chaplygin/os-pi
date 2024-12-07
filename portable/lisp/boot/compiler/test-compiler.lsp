;; Тест компиляции программы.
(defun test-compile (expr expected-res)
  (let ((program (compile expr)))
    (print program)
    (print (assert program expected-res))))

(test-compile '(progn 1 2 3) '(seq (const 1) (seq (const 2) (const 3))))
(test-compile '(progn) '(const ()))
(test-compile '(if 1 (progn 1 3) (progn 2)) '(ALTER (CONST 1) (SEQ (CONST 1) (CONST 3)) (CONST 2)))
(test-compile '(if t t nil) '(ALTER (GLOBAL-REF 0) (GLOBAL-REF 0) (GLOBAL-REF 1)))
(test-compile '(progn (setq a 1) (setq b 2) a b) '(SEQ (GLOBAL-SET 2 (CONST 1)) (SEQ (GLOBAL-SET 3 (CONST 2)) (SEQ (GLOBAL-REF 2) (GLOBAL-REF 3)))))
(test-compile '(defun test (x) (progn (setq a 2) (setq x a) x)) '(LABEL TEST (FIX-CLOSURE 1 (SEQ (GLOBAL-SET 2 (CONST 2)) (SEQ (LOCAL-SET 0 (GLOBAL-REF 2)) (LOCAL-REF 0))))))
(test-compile '((defun test (x) x x) (test 10)) '(SEQ (LABEL TEST (FIX-CLOSURE 1 (LOCAL-REF 0))) (SEQ (ALLOC 1) (LOCAL-SET 0 (CONST 10)) (REG-CALL TEST))))

;(test-compile '(progn (defun test (x y) (progn x y)) (test 10 (if t 3 4))) '())
