;; *test-failed* - флаг - провалился ли хотя бы один тест.
(defvar *test-failed* nil)
;; (setq *test-failed* t)

;; Тест компиляции, ассемблирования и выполнения программы.
(defun test (expr expected-res)
  (unless (or *test-failed*
	      *test-compile-failed*)
    (let* ((program (compile expr))
           (bytecode (assemble program))
           (target nil))
      (if *comp-err*
          (setq target *comp-err-msg*)
	      (setq target (vm-run bytecode)))
      (let ((res (assert target expected-res)))
	    (print expr)
	    (when (not (null program)) (print program))
	    (when (> (array-size bytecode) 0) (print bytecode))
	    (print res)
        (if (eq (car res) 'fail)
	        (setq *test-failed* t)
            (print '---------------------))))))

;; Проверка, все ли тесты успешны
(defun check-tests ()
  (when (null *test-failed*)
    (print "All tests OK")))


(test '(progn 1 2 3) 3)
(test '(progn (progn 2 (progn 11) 4) (progn 3)) 3)
(test '(progn) nil)
(test '(progn (progn (progn) (progn 1)) 2) 2)

(test '(if) "[Compilation error] if: no params")
(test '(if t) "[Compilation error] if: not enough params")
(test '(if t 1) "[Compilation error] if: not enough params")
(test '(if t 1 2 3) "[Compilation error] if: too many params")
(test '(if t 1 2) 1)
(test '(if 5 1 2) 2)
(test `(if ,nil 1 2) 2)
(test '(if t (if t 1 2) 3) 1)
(test `(if t (if ,nil 1 2) 3) 2)
(test `(if ,nil (if ,nil 1 2) 3) 3)

(test 'a "[Compilation error] Unknown symbol: A")
(test '(setq) "[Compilation error] setq: no params")
(test '(setq a) "[Compilation error] setq: no expression to set")
(test '(setq a 1 b) "[Compilation error] setq: no expression to set")
(test '(setq 1 1) "[Compilation error] setq: variable name is not a symbol")
(test '(setq (a) 1) "[Compilation error] setq: variable name is not a symbol")
(test '(setq a 1 (b) 2) "[Compilation error] setq: variable name is not a symbol")
(test '(setq t 1) "[Compilation error] setq: variable name is constant: T")
(test `(setq ,nil 1) "[Compilation error] setq: variable name is not a symbol")
(test '(setq a 5) 5)
(test '(progn (setq a 5) a) 5)
(test '(progn (setq a 10) (setq b a) b) 10)
(test '(progn (setq a (progn)) a) nil)
(test '(setq a 5 b 10) 10)
(test '(progn (setq a 5 b 10) a) 5)
(test '(progn (setq a 5 b a) b) 5)

(test '(lambda) "[Compilation error] Unknown func: LAMBDA")
(test '((lambda)) "[Compilation error] No params in lambda")
(test '((lambda ())) "[Compilation error] No body in lambda")
(test '((lambda (5) 5)) "[Compilation error] Not symbol in lambda args")
(test '((lambda (x) x)) "[Compilation error] Invalid number of arguments (expected 1, but got 0)")
(test '((lambda () x)) "[Compilation error] Unknown symbol: X")
(test '((lambda () (setq x 5))) 5)
(test '((lambda (x) x) 5) 5)
(test '((lambda () 10)) 10)
(test '((lambda (x) ((lambda (y) y) 20)) 15) 20)
(test '((lambda (x) ((lambda () x))) 25) 25)
(test '((lambda (x) (setq x 35)) 30) 35)

(test '(progn
	    (defun f (x) x)
	    (f 5))
      5)
(test '(progn
        (defun f (x) x)
        (defun f () 1)
        (f))
      1)

(test '(+ 5 10) 15)
(test '((lambda (a b) (+ a b)) 15 20) 35)
(test '(progn
        (defun fact (x)
          (if (> x 1)
              (* x (fact (- x 1)))
              x))
        (fact 3))
      6)

(test '(progn
	 (defun test (x)
	   ((lambda (y) (+ x y)) 10))
	 (test 20))
      30)
(test '(progn
	 (defun test (x)
	   ((lambda (y) (+ x y)) 10)
	   ((lambda (y z) (+ x (+ y z))) 20 30))
	 (test 50))
      100)


(check-tests)
