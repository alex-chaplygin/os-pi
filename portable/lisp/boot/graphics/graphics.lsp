(defvar +screen-width+ 320)
(defvar +screen-height+ 200)
(defvar +screen-depth+ 8)
(defvar *gr-buf* (make-array (* +screen-width+ +screen-height+ (/ +screen-depth+ 8))))

(defun set-pixel (x y colour)
  "Установка пикселя"
  "x y - координаты пикселя, colour - цвет"
  (if (< x +screen-width+)
    (if (< y +screen-height+)
      (seta *gr-buf* (+ x (* y +screen-width+)) colour) nil) nil))

(defun draw-line (x1 y1 x2 y2 colour)
  "Рисование линии"
  "x1 y1 x2 y2 - координаты начала и конца линии, colour - цвет"
    (setq dx (abs (- x2 x1)))
    (setq dy (abs (- y2 y1)))
    (if (< x1 x2)
      (setq signx 1)
      (setq signx (- 0 1)))
    (if (< y1 y2)
      (setq signy 1)
      (setq signy (- 0 1)))
    (setq error1 (- dx dy))
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
        nil))))

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

(defun draw-circle (xi yi r colour)
  "Рисование окружности (не доделано)"
  "x y - координаты центра, r - радиус, colour - цвет"
  (defun intern-circle (f x y x0 y0 ddf_x ddf_y colour)
    (if (>= f 0)
      (progn
        (setq y (- y 1))
        (setq ddf_y (+ ddf_y 2))
        (setq f (+ f ddf_y)))
      nil)
    (setq x (+ x 1))
    (setq ddf_x (+ ddf_x 2))
    (setq f (+ f ddf_x))
    (set-pixel (+ x0 x) (+ y0 y) colour)
    (set-pixel (+ x0 x) (- y0 y) colour)
    (set-pixel (- x0 x) (+ y0 y) colour)
    (set-pixel (- x0 x) (- y0 y) colour)
    (set-pixel (+ x0 y) (+ y0 x) colour)
    (set-pixel (+ x0 y) (- y0 x) colour)
    (set-pixel (- x0 y) (+ y0 x) colour)
    (set-pixel (- x0 y) (- y0 x) colour)
    (if (< x y) (intern-circle f x y x0 y0 ddf_x ddf_y colour) nil))
  (setq x0 xi)
  (setq y0 yi)
  (setq radius r)
  (setq f (- 1 radius))
  (setq ddf_x 1)
  (setq ddf_y (- (* 2 radius)))
  (setq x 0)
  (setq y radius)
  (set-pixel x0 (+ y0 radius) colour)
  (set-pixel x0 (- y0 radius) colour)
  (set-pixel (+ x0 radius) y0 colour)
  (set-pixel (- x0 radius) y0 colour)
  (intern-circle f x y x0 y0 ddf_x ddf_y colour))

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
