
(defun test-expt (x y res)
  (print (assert (expt x y) res)))
	  

(test-expt 10 3 1000)
(test-expt (/ 1.0 10.0) 3 (/ 1.0 1000.0))

