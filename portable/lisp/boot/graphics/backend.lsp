(defvar +screen-width+ 320)
(defvar +screen-height+ 200)
(defvar +screen-depth+ 8)
(defvar *gr-buf* (make-array (* +screen-width+ +screen-height+ (/ +screen-depth+ 8))))

(defun set-pixel (x y colour)
  "Установка пикселя"
  "x y - координаты пикселя, colour - цвет"
  (if (< x +screen-width+)
    (if (> x -1)
      (if (< y +screen-height+)
        (if (> y -1)
          (seta *gr-buf* (+ x (* y +screen-width+)) colour)
          nil) nil) nil) nil))

(defun draw-line (x1 y1 x2 y2 colour)
  "Рисование линии"
  "x1 y1 x2 y2 - координаты начала и конца линии, colour - цвет"
  (let* ((dx (abs (- x2 x1)))
         (dy (abs (- y2 y1)))
         (signx (if (< x1 x2) 1 (- 0 1)))
         (signy (if (< y1 y2) 1 (- 0 1)))
         (error1 (- (abs (- x2 x1)) (abs (- y2 y1))))
         (error2 0))
    (set-pixel x2 y2 colour)
    (while (or (not (= x1 x2)) (not (= y1 y2)))
      (progn
        (set-pixel x1 y1 colour)
        (setq error2 (* error1 2))
        (if (> error2 (- 0 dy))
          (progn
            (setq error1 (- error1 dy))
            (setq x1 (+ x1 signx)))
        nil)
        (if (< error2 dx)
          (progn
            (setq error1 (+ error1 dx))
            (setq y1 (+ y1 signy)))
        nil)))))

(defun draw-rect (x y w h colour)
  "Рисование полого прямоугольника"
  "x y - координаты левого верхнего угла, w h - ширина и высота, colour - цвет"
  (draw-line x y (+ x w) y colour)
  (draw-line (+ x w) y (+ x w) (+ y h) colour)
  (draw-line (+ x w) (+ y h) x (+ y h) colour)
  (draw-line x (+ y h) x y colour))

(defun draw-rectf (x y w h colour)
  "Рисование заполненного прямоугольника"
  "x y - координаты левого верхнего угла, w h - ширина и высота, colour - цвет"
  (for i 0 h
    (for j 0 w
      (set-pixel (+ x j) (+ y i) colour))))

(defun draw-sym-pixels (cx cy x y colour)
  (set-pixel (+ cx x) (+ cy y) colour)
  (set-pixel (- cx x) (+ cy y) colour)
  (set-pixel (+ cx x) (- cy y) colour)
  (set-pixel (- cx x) (- cy y) colour))

(defun draw-circle (cx cy r colour)
  "Рисование окружности"
  "x y - координаты центра, r - радиус, colour - цвет"
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

(defun bezier-curve (p0 p1 p2 p3 ti)
  (cons (/ (+ (* (expt (- 100 ti) 3) (car p0)) (* 3 ti (expt (- 100 ti) 2) (car p1)) (* 3 (expt ti 2) (- 100 ti) (car p2)) (* (expt ti 3) (car p3))) 1000000)
        (/ (+ (* (expt (- 100 ti) 3) (cdr p0)) (* 3 ti (expt (- 100 ti) 2) (cdr p1)) (* 3 (expt ti 2) (- 100 ti) (cdr p2)) (* (expt ti 3) (cdr p3))) 1000000)))

(defun draw-bezier-curve (x1 y1 x2 y2 x3 y3 x4 y4 colour)
  "Рисование кривой Безье"
  "x1 y1 x2 y2 x3 y3 x4 y4 - координаты точек, colour - цвет"
  (for ti 0 100
    (let ((curve-point (bezier-curve (cons ,x1 ,y1) (cons ,x2 ,y2) (cons ,x3 ,y3) (cons ,x4 ,y4) ti)))
      (set-pixel (car curve-point) (cdr curve-point) colour))))

(defun gr-test ()
  "Тест графики"
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (for y 0 +screen-height+
  (for x 0 +screen-width+
    ; (seta *gr-buf* (+ x (* y +screen-width+)) (& x 0xff))))
    (set-pixel x y (& x 0xff))))
  (graph-send-buffer *gr-buf*))

(defun rect-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (draw-rect 30 10 200 100 1)
  (graph-send-buffer *gr-buf*))

(defun rectf-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (draw-rectf 30 10 200 100 1)
  (graph-send-buffer *gr-buf*))

(defun line-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (draw-line 37 17 176 200 1)
  (draw-line 200 13 20 100 3)
  (draw-line 72 126 13 200 2)
  (graph-send-buffer *gr-buf*))

(defun circle-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (draw-circle 100 100 20 1)
  (graph-send-buffer *gr-buf*))

(defun bezier-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (draw-bezier-curve 10 10 100 100 200 100 300 10 1)
  (graph-send-buffer *gr-buf*))