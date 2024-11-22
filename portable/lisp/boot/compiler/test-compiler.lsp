;; *test-failed* - флаг - провалился ли хотя бы один тест.
(defvar *test-compile-failed* nil)


;; Тест компиляции, ассемблирования и выполнения программы.
(defun test-compile (expr expected-res)
  (unless *test-compile-failed*
    (let* ((program (compile expr))
	   (target (if *comp-err*
		       *comp-err-msg*
		     program))
	   (res (assert target expected-res)))
      (print expr)
      (print target)
      (print res)
      (if (eq (car res) 'fail)
	  (setq *test-compile-failed* t)
	(print '---------------------)))))

;; Проверка, все ли тесты успешны
(defun check-compiler-tests ()
  (when (null *test-compile-failed*)
    (print "\n\nAll compiler tests OK\n\n")))


(test-compile 1 '((lda 1)))
(test-compile '(progn 1 2 3) '((lda 1) (lda 2) (lda 3)))
(test-compile '(progn) '())


(check-compiler-tests)
