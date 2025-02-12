(defun test-expt (x y res)
  (print (assert (expt x y) res)))
	  
(defun test-randint (min max iterations)
  (let ((count-table (make-hash)))
    (for i 0 iterations
         (let ((random-number (randint min max)))
	   (print random-number)
           (if (not (check-key count-table random-number)) 
             (set-hash count-table random-number 0)
           (set-hash count-table random-number (+ (get-hash count-table random-number) 1)))))
    (print count-table)))

(defun abs-tests ()
  "Тестирование функции abs"
  (print "Тестирование функции abs")
  (print (assert (abs -5) 5))   ; тест: отрицательное число
  (print (assert (abs 5) 5))    ; тест: положительное число
  (print (assert (abs 0) 0)))    ; тест: ноль


(test-expt 10 3 1000)
(test-expt (/ 1.0 10.0) 3 (/ 1.0 1000.0))
(test-expt 2 0 1)
(test-expt 0 5 0)
(test-expt 5 1 5)
(test-expt -2 3 -8)
(test-expt -2 4 16)

(test-randint 0 3 10)
(abs-tests)


