(defun test-make-vec2 (x y)
  (let ((vec (make-vec2 x y)))
    (print (assert (vec2-x vec) x))
    (print (assert (vec2-y vec) y))))

(defun test-vec2-add (x1 y1 x2 y2)
  (let* ((vec1 (make-vec2 x1 y1))
	(vec2 (make-vec2 x2 y2))
	(result-vec (vec2-add vec1 vec2)))
    (print (assert (vec2-x result-vec) (+ x1 x2)))
    (print (assert (vec2-y result-vec) (+ y1 y2)))))

(defun test-vec2-scale (x y s)
  (let ((vec (vec2-scale (make-vec2 x y) s)))
    (print (assert (vec2-x vec) (* x s)))
    (print (assert (vec2-y vec) (* y s)))))

(test-make-vec2 2 5)
(test-make-vec2 4.1 2)
(test-make-vec2 1 2.5)
(test-make-vec2 4.1 10.2)
(test-vec2-add 4 6 6 10)
(test-vec2-add 4.1 2.1 6 10)
(test-vec2-add 10 0 6.6 50.1)
(test-vec2-add 4.3 23.4 29.5 150.3)
(test-vec2-scale 2 4 6)
(test-vec2-scale 3.1 2 10)
(test-vec2-scale 23 32.6 5)
(test-vec2-scale 2.14 10.11 3)
(test-vec2-scale 2.5 4.1 10.5)
