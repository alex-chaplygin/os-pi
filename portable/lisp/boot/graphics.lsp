(defvar +screen-width+ 320)
(defvar +screen-height+ 200)
(defvar +screen-depth+ 8)
(defvar *gr-buf* (make-array (* +screen-width+ +screen-height+ (/ +screen-depth+ 8))))

(defun set-pixel (x y colour)
  "Установка пикселя"
  "x y - координаты пикселя, colour - цвет"
  (seta *gr-buf* (+ x (* y +screen-width+)) colour))

(defun draw-line (x1 y1 x2 y2 colour)
  "Рисование линии"
  "x1 y1 x2 y2 - координаты начала и конца линии, colour - цвет"
  (for x (min x1 x2) (max x1 x2) (
    (setq m (/ (- y2 y1) (- x2 x1)))
    (setq b (- y1 (* m x1)))
    (setq y (+ (* m x) b))
    (set-pixel x y colour))))

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
    (draw-line x (+ y i) (+ x w) (+ y i) colour)))

(defun gr-test ()
  "Тест графики"
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (for y 0 +screen-height+
	 (for x 0 +screen-width+
	      (seta *gr-buf* (+ x (* y +screen-width+)) (& x 0xff))))
  (graph-send-buffer *gr-buf*))