(defvar *screen-width*) ; ширина экрана
(defvar *screen-height*) ; высота экрана
(defvar +screen-depth+ 8) ; число бит на пиксель
(defvar *graphics-buffer*) ; внутренний буфер пикселей

(defclass Edge ()
  (cur-x ; текущее пересечение (начало ребра)
   cur-y ; текущая горизонталь
   end-y ; конечная горизонталь
   dx ; разница координат ребра по x
   dy ; разница координат ребра по y
   counter)) ; счетчик для дроби

(defun draw-screen ()
  "Перерисовка экрана"
  (let*  ((x (vec2-x *draw-top-left*))
	  (y (vec2-y *draw-top-left*))
	  (width (- (vec2-x *draw-bottom-right*) x))
	  (height (- (vec2-y *draw-bottom-right*) y)))
    (when (and (> width 0) (> height 0))
      (send-graphics-buffer *graphics-buffer* x y width height)))
  (screen-reset))

(defun clear-screen ()
  "Очистка экрана"
  (let ((size (* *screen-height* *screen-width*)))
    (for i 0 size
	 (seta *graphics-buffer* i 0)))
  (graph-send-buffer *graphics-buffer*))

(defun init-screen (w h)
  "Инициализация экрана с разрешением w на h"
  (setq *screen-width* w)
  (setq *screen-height* h)
  (setq *graphics-buffer* (make-array (* w h (/ +screen-depth+ 8))))
  (bgr-set-res w h +screen-depth+)
  (clear-screen)
  (screen-reset))

(defun set-pixel (x y colour)
  "Установка пикселя"
  "x y - координаты пикселя, colour - цвет"
  (seta *graphics-buffer* (+ x (* y *screen-width*)) colour))

(defun mid-point (x1 y1 x2 y2)
  "Алгоритм средней точки"
  (let* ((dx (abs (- x2 x1)))
         (dy (abs (- y2 y1)))
         (signx (if (< x1 x2) 1 -1))
         (signy (if (< y1 y2) 1 -1))
         (error1 (- dx dy))
         (points nil))
    (while (or (not (= x1 x2)) (not (= y1 y2)))
      (setq points (append points (list (cons x1 y1))))
      (let ((error2 (* 2 error1)))
        (when (> error2 (- 0 dy))
          (setq error1 (- error1 dy))
          (setq x1 (+ x1 signx)))
        (when (< error2 dx)
          (setq error1 (+ error1 dx))
          (setq y1 (+ y1 signy)))))
    (setq points (append points (list (cons x2 y2))))))

(defun draw-line (x1 y1 x2 y2 colour)
  "Рисование линии"
  (screen-add-rect 
   (cons (minp #'< (list x1 x2)) (minp #'< (list y1 y2)))
   (cons (abs (- x2 x1)) (abs (- y2 y1))))
  (let ((points (mid-point x1 y1 x2 y2)))
    (dolist (p points)
      (set-pixel (car p) (cdr p) colour))))

(defun draw-hline (x1 x2 y colour)
  (screen-add-rect (cons x1 y) (cons (- x2 x1) 1))  
  "Рисование горизонтальной линии"
  (for xx x1 x2
       (set-pixel xx y colour)))

(defun next-x (edge)
  "Поиск пересечений горизонтали с ребром"
  (let ((counter (slot edge 'counter))
        (cur-x (slot edge 'cur-x))
	(cur-y (slot edge 'cur-y))
        (dx (slot edge 'dx))
        (dy (slot edge 'dy)))
    (setq counter (+ counter (* (abs dx) 2)))
    (while (>= counter dy)
      (setq counter (- counter (* dy 2)))
      (setq cur-x (+ cur-x (if (< dx 0) -1 1))))
    (setf (slot edge 'counter) counter)
    (setf (slot edge 'cur-x) cur-x)
    (setf (slot edge 'cur-y) (++ cur-y))
    cur-x))

(defun fill-triangle (p1 p2 p3 colour)
  "Рисование залитого треугольника"
  (labels ((fill (edge1 edge2)
		 (let ((x1 (slot edge1 'cur-x))
		       (x2 (slot edge2 'cur-x))
		       (y1 (slot edge1 'cur-y)))
		   (when (> (- x2 x1) 1)
		     (draw-hline (++ x1) (-- x2) y1 colour))
		   (while (< (++ y1) (slot edge1 'end-y))
		     (setq y1 (++ y1))
		     (setq x1 (next-x edge1))
		     (setq x2 (next-x edge2))
		     (when (> (- x2 x1) 1)
		       (draw-hline (++ x1) (-- x2) y1 colour))))
		 (next-x edge1)
		 (next-x edge2)))
	  (let* ((cords (sort #'(lambda (p1 p2)
				  (let ((y1 (cdr p1))
					(y2 (cdr p2)))
				    (if (= y1 y2) (< (car p1) (car p2))
				      (< y1 y2)))) (list p1 p2 p3)))
		 (pp1 (car cords))
		 (pp2 (cadr cords))
		 (pp3 (caddr cords))
		 (x1 (car pp1))
		 (y1 (cdr pp1))
		 (x2 (car pp2))
		 (y2 (cdr pp2))
		 (x3 (car pp3))
		 (y3 (cdr pp3))
		 (dxp1p2 (- x2 x1))
		 (dyp1p2 (- y2 y1))
		 (dxp1p3 (- x3 x1))
		 (dyp1p3 (- y3 y1))
		 (dxp2p3 (- x3 x2))
		 (dyp2p3 (- y3 y2))
		 (p1p2 (make-Edge x1 y1 y2 dxp1p2 dyp1p2 0))
		 (p1p3 (make-Edge x1 y1 y3 dxp1p3 dyp1p3 0))
		 (p2p3 (make-Edge x2 y2 y3 dxp2p3 dyp2p3 0)))
	    (screen-add-rect (cons x1 y1) (cons (- x3 x1) (- y3 y1)))
	    (when (> y2 y1) (if (< dxp1p2 dxp1p3) (fill p1p2 p1p3) (fill p1p3 p1p2)))
	    (when (= y1 y2) (next-x p2p3) (next-x p1p3))
	    (when (> y3 y2) (if (< dxp1p3 dxp2p3) (fill p2p3 p1p3) (fill p1p3 p2p3))))))

(defun draw-rect (x y w h colour)
  "Рисование полого прямоугольника"
  "x y - координаты левого верхнего угла, w h - ширина и высота, colour - цвет"
  (screen-add-rect (cons x y) (cons (++ w) (++ h)))  
  (draw-hline x (+ x w) y colour)
  (draw-line (+ x w) y (+ x w) (+ y h) colour)
  (draw-hline x (+ x w) (+ y h) colour)
  (draw-line x (+ y h) x y colour))

(defun draw-rectf (x y w h colour)
  "Рисование заполненного прямоугольника"
  "x y - координаты левого верхнего угла, w h - ширина и высота, colour - цвет"
  (screen-add-rect (cons x y) (cons w h))   
  (for i 0 h
       (draw-hline x (+ x w) (+ y i) colour)))

(defun draw-sym-pixels (cx cy x y colour)
  (set-pixel (+ cx x) (+ cy y) colour)
  (set-pixel (- cx x) (+ cy y) colour)
  (set-pixel (+ cx x) (- cy y) colour)
  (set-pixel (- cx x) (- cy y) colour))

(defun draw-circle (cx cy r colour)
  "Рисование окружности"
  "x y - координаты центра, r - радиус, colour - цвет"
  (screen-add-rect (cons (- cx r) (- cy r)) (cons (* 2 r) (* 2 r)))
  (defun inner-draw-circle (cx cy r colour x y delta error)
    (draw-sym-pixels cx cy x y colour)
    (draw-sym-pixels cx cy y x colour)
    (setq error (- (* 2 (+ delta y)) 1))
    (if (and (< delta 0) (<= error 0))
	(progn
	  (setq delta (+ delta (+ (* 2 (+ x 1)) 1)))
	  (setq x (+ x 1)))
      (progn
        (setq error (- (* 2 (- delta x)) 1))
        (if (and (> delta 0) (> error 0))
	    (progn
	      (setq delta (+ delta (- 1 (* 2 (- y 1)))))
	      (setq y (- y 1)))
          (progn
            (setq delta (+ delta (* 2 (+ 1 (- x y)))))
            (setq x (+ x 1))
            (setq y (- y 1))))))
    (if (>= y x)
	(inner-draw-circle cx cy r colour x y delta error) nil))
  (let* ((x 0)
         (y r)
         (delta (- (* 2 r)))
         (error 0))
    (inner-draw-circle cx cy r colour x y delta error)))

(defun bezier-point (p0 p1 p2 p3 ti)
  "Возвращает очередную точку кривой"
  (let* ((t1 (- 1 ti))
         (t2 (expt t1 2))
         (t3 (expt t1 3))
	 (ti2 (expt ti 2))
	 (ti3 (expt ti 3)))
    (cons (+ (* t3 (car p0)) (* 3 ti t2 (car p1)) (* 3 t1 ti2 (car p2)) (* ti3 (car p3)))
	  (+ (* t3 (cdr p0)) (* 3 ti t2 (cdr p1)) (* 3 t1 ti2 (cdr p2)) (* ti3 (cdr p3))))))

(defun draw-bezier-curve (p1 p2 p3 p4 n colour)
  "Рисование кривой Безье"
  "p1 p2 p3 p4 - точки, n - число точек в кривой colour - цвет"
  (let* ((ti 0.0)
	 (step (/ 1.0 n))
	 (fin-ti (+ 1.0 step)))
    (while (< ti fin-ti)
      (let ((curve-point (bezier-point p1 p2 p3 p4 ti)))
	(set-pixel (round (car curve-point)) (round (cdr curve-point)) colour))
      (setq ti (+ ti step)))))

(defun draw-image (image)
  "Вывод изображения в позиции матрицы трансформации"
  (let* ((ctm (get-hash *cur-state* 'ctm))
	 (p (mat-mul-vec ctm '(0 . 0)))
	 (ofs (+ (* (vec-y p) *screen-width*) (vec-x p) -1))
	 (width (image-width image))
	 (height (image-height image)))
    (for y 0 height
	 (let ((row (image-row y)))
	   (for x 0 width
		(seta *graphics-buffer* ofs (aref row x))
		(incf ofs))
	   (setq ofs (+ ofs *screen-width* (- 0 width)))))))
