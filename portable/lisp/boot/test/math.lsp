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

(test-expt 10 3 1000)
(test-expt (/ 1.0 10.0) 3 (/ 1.0 1000.0))

(test-randint 0 3 10)


