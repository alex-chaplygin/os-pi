;; *test-failed* - флаг - провалился ли хотя бы один тест.
(defvar *test-failed*)
;; *started-tests-count* - количество запущенных тестов.
(defvar *started-tests-count* 0)
;; *started-tests-count* - количество успешных тестов.
(defvar *ok-tests-count* 0)


;; Тест компиляции, ассемблирования и выполнения программы.
(defun test (expr expected-res)
  (unless (= *started-tests-count* *ok-tests-count*)
    (setq *test-failed* t))
  (incf *started-tests-count*)
  (unless *test-failed*
    (print "Expression")
    (print expr)
    (let* ((tree (compile expr))
           (asmcode (generate tree))
           (bytecode (assemble asmcode))
           (res (vm-run bytecode))
           ;; (res nil)
           )
      (print "Compiler")
      (print tree)
      (print "Generator")
      (print asmcode)
      (print "Assembler")
      (print bytecode)
      (print "Execution")
      (print res)
      (let ((res (assert res expected-res)))
        (print res)
        (if (eq (car res) 'fail)
            (setq *test-failed* t)
            (progn
              (incf *ok-tests-count*)
              (print '---------------------)))))))

;; Проверка, все ли тесты успешны
(defun check-tests ()
  (when (and (= *started-tests-count* *ok-tests-count*)
             (null *test-failed*))
    (print "All tests OK")))


(test '(progn 1 2 3) 3)
(test '(progn (progn 2 (progn 11) 4) (progn 3)) 3)
(test '(progn) nil)
(test '(progn (progn (progn) (progn 1)) 2) 2)

(test '(if t 1 2) 1)
(test '(if 5 1 2) 1)
(test `(if ,nil 1 2) 2)
(test '(if t (if t 1 2) 3) 1)
(test `(if t (if ,nil 1 2) 3) 2)
(test `(if ,nil (if ,nil 1 2) 3) 3)

(test '(setq a 5) 5)
(test '(progn (setq a 5) a) 5)
(test '(progn (setq a 10) (setq b a) b) 10)
(test '(progn (setq a (progn)) a) nil)
(test '(setq a 5 b 10) 10)
(test '(progn (setq a 5 b 10) a) 5)
(test '(progn (setq a 5 b a) b) 5)

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

(test '(+ 5 10) 15)
(test '((lambda (a b) (+ a b)) 15 20) 35)
(test '(progn
        (defun fact (x)
          (if (> x 1)
              (* x (fact (- x 1)))
              x))
        (fact 2))
      2)

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

(test '(progn (setq a 1 b 2) `(a (,a) b (,b))) '(a (1) b (2)))


(check-tests)
