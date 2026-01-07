(defun jpeg-hufs (scan)
  "Получить таблицы Хаффмана для всех каналов"
  (array-map scan #'(lambda (x) (let ((d-a (get-hash x 'tda)))
				  (cons (jpeg-huf-dc (car d-a)) (jpeg-huf-ac (cdr d-a)))))))
;; параметры HV всех каналов
(defun jpeg-hv (params) (array-map params #'(lambda (x) (get-hash x 'hv))))

(defun jpeg-quants (params)
  "Получить таблицы квантования для всех каналов"
  (array-map params #'(lambda (x) (jpeg-quant (get-hash x 'tq)))))

(defun get-bit (self)
  "Специальная для JPEG функция чтения бита"
  (let ((arr (astream-arr self))
	(bit-num (astream-bit-num self))
        (byte-num (astream-byte-num self)))
    (when (= bit-num 0)
      (when (and (> byte-num 0) (= (aref arr byte-num) 0xff))
	(if (= (aref arr (++ byte-num)) 0)
	    (setq byte-num (++ byte-num))
	    (error "jpeg: get-bit 0xFF != 00")))
      (setq byte-num (++ byte-num))
      (setq bit-num 8))
    (let ((new-bit-num (-- bit-num))
	  (cur-byte (aref arr byte-num)))
      (if (= byte-num (array-size arr)) nil
	  (cons (& 1 (>> cur-byte new-bit-num))
		(make-astream arr byte-num new-bit-num t))))))

(defun compute-mcu (frame)
  "Вычислить необходимые параметры для MCU"
  (labels ((jpeg-size (s m) (if (= (% s m) 0) s (* (++ (/ s m)) m))))
    (let* ((st (car frame))
	   (par (cdr frame))
	   (hv (jpeg-hv par))
	   (width (get-hash st 'x))
	   (height (get-hash st 'y))
	   (hvy (aref hv 0))
	   (mcu-width (<< (car hvy) 3))
	   (mcu-height (<< (cdr hvy) 3))
	   (width2 (jpeg-size width mcu-width))
	   (height2 (jpeg-size height mcu-height))
	   (mcu-hcount (/ width2 mcu-width))
	   (mcu-vcount (/ height2 mcu-height)))
      ;;(print `(mcu-width ,mcu-width mcu-height ,mcu-height width2 ,width2 height2 ,height2 mcu-hcount ,mcu-hcount mcu-vcount ,mcu-vcount))
      (set-hash st 'width2 width2) ;; внутренняя ширина
      (set-hash st 'height2 height2) ;; внутренняя высота
      (set-hash st 'jpeg-hv (jpeg-hv par)) ;; коэффициенты прореживания
      (set-hash st 'mcu-width mcu-width) ;; ширина MCU
      (set-hash st 'mcu-height mcu-height) ;; высота MCU
      (set-hash st 'mcu-hcount mcu-hcount) ;; число MCU по горизонтали
      (set-hash st 'mcu-vcount mcu-vcount) ;; число MCU по вертикали
      (* mcu-hcount mcu-vcount))))

(defun put-block (bl arr bx by)
  "Записать блок в матрицу arr"
  (let ((posy by)
	(h (array-size bl))
	(w (array-size (aref bl 0))))
    (for j 0 h
	 (let ((row (aref arr posy))
	       (blrow (aref bl j))
	       (posx bx))
	   (for i 0 w
		(seta row posx (aref blrow i))
		(incf posx)))
	 (incf posy))))

(defun scale (bl kx ky)
  "Масштабирование блока bl с коэффициентами kx ky, линейная интерполяция"
  (labels ((lerp (x x0 y0 x1 y1) (clamp (round (+ y0 (* (/ (- y1 y0) (- x1 x0)) (- x x0)))))))
    (let* ((new-w (<< kx 3))
	   (new-h (<< ky 3))
	   (new-bl (make-array new-h))
	   (posx 0)
	   (x0 0)
	   (stepx (/ 1.0 kx))
	   (stepy (/ 1.0 ky))
	   (fx stepx))
      (for y 0 new-h
	   (seta new-bl y (make-array new-w)))
      (for x 0 new-w
	   (setq x0 (/ x kx))
	   (when (= x0 7) (decf x0))
	   (let ((fx (+ (* x0 kx) stepx)))
	     (for y 0 new-h
		  (let ((row (aref bl (/ y ky))))
		    (seta (aref new-bl y) posx (lerp x fx (aref row x0) (+ fx kx) (aref row (++ x0)))))))
	   (incf posx))
      (for y 0 new-h
	   (setq y0 (/ y ky))
	   (when (= y0 7) (decf y0))
	   (let ((fy (+ (* y0 ky) stepy))
		 (row (aref new-bl y))
		 (row0 (aref new-bl (* y0 ky)))
		 (row1 (aref new-bl (* (++ y0) ky))))
	     (for x 0 new-w
		  (seta row x (lerp y fy (aref row0 x) (+ fy ky) (aref row1 x))))))
      new-bl)))

(defun put-mcu (mcu y cb cr x yy hvs)
  "Записать MCU в каналы Y Cb Cr по координатам x yy"
  (let* ((hv0 (aref hvs 0))
	 (h0 (car hv0))
	 (v0 (cdr hv0)))
    (for i 0 3
	 (let* ((hv (aref hvs i))
		(h (car hv))
		(v (cdr hv))
		(blocks (car mcu))
		(by yy))
	   (if (or (!= h h0) (!= v v0))
	       (put-block (scale (car blocks) (/ h0 h) (/ v0 v)) (case i (0 y) (1 cb) (2 cr)) x yy)
	   (for my 0 v
		(let ((bx x))
		  (for mx 0 h
		       (put-block (car blocks) (case i (0 y) (1 cb) (2 cr)) bx by)
		       (setq bx (+ bx 8) blocks (cdr blocks))))
		(setq by (+ by 8)))))
	 (setq mcu (cdr mcu)))))

(defun ycbcr-to-rgb (my cb cr width height)
  "Преобразует 3 матрицы Y Cb Cr в одну матрицу RGB с заданными размерами"
  (labels ((pix (m x y) (aref (aref m y) x)))
    (let* ((matrix (make-array height)))
      (for y 0 height
	   (let ((row (make-array (* width 3)))
		 (pos 0))
	     (for x 0 width
		  (let ((yy (pix my x y))
			(cbb (- (pix cb x y) 0x80))
			(crr (- (pix cr x y) 0x80)))
		    (seta row pos (clamp (round (+ yy (* 1.40200 crr)))))
		    (incf pos)
		    (seta row pos (clamp (round (- yy (* 0.34414 cbb) (* 0.71414 crr)))))
		    (incf pos)
		    (seta row pos (clamp (round (+ yy (* 1.77200 cbb)))))
		    (incf pos)))
	     (seta matrix y row)))
      matrix)))
