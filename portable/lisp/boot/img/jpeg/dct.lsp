(defconst +zig-zag+ #(#(0 1 5 6 14 15 27 28)
		    #(2 4 7 13 16 26 29 42)
		    #(3 8 12 17 25 30 41 43)
		    #(9 11 18 24 31 40 44 53)
		    #(10 19 23 32 39 45 52 54)
		    #(20 22 33 38 46 51 55 60)
		    #(21 34 37 47 50 56 59 61)
		    #(35 36 48 49 57 58 62 63)))

(defun IDCT (table)
  "Обратное дискретное косинусное преобразование Вход: двумерный массив 8x8 Выход: двумерный массив 8x8"
  (let ((sum 0)
	(result (make-array 8)))
    (for i 0 8
	 (seta result i (make-array 8)))
    (for y 0 8
	 (for x 0 8
	      (setq sum 0)
	      (for v 0 8
		   (for u 0 8
			(setq sum (+ sum
				     (* (if (= u 0) (/ 1.0 (sqrt 2.0)) 1.0)
					  (if (= v 0) (/ 1.0 (sqrt 2.0)) 1.0)
					  (aref (aref table v) u)
					  (cos (/ (* PI u (+ (* 2 x) 1)) 16.0))
					  (cos (/ (* PI v (+ (* 2 y) 1)) 16.0)))))))
	      (seta (aref result y) x (round (* 0.25 sum)))))
    result))

(defun unzip (coef)
  (let ((result (make-array 8)))
    (for i 0 8
	 (seta result i (make-array 8)))
    (for x 0 8
	 (for y 0 8
	      (seta (aref result x) y (aref coef (aref (aref +zig-zag+ x) y)))))
    result))

(defun dequant (coef quant_table)
  (for i 0 64
       (seta coef i (* (aref coef i) (aref quant_table i))))
  coef)

(defun clamp (num)
  (let ((res num))
    (if (> res 255) (setq res 255) (if (< res 0) (setq res 0) nil))
    res))

(defun level-shift (block)
  (for i 0 8
       (for j 0 8
	    (seta (aref block i) j (clamp (+ 128 (aref (aref block i) j))))))
    block)
