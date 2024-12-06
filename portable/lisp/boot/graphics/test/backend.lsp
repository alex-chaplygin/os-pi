(defun draw-screen-test ()
  "Тестирование вывода экрана"
  (let ((i -1))
    (for y 0 *screen-height*
	 (for x 0 *screen-width*
             (seta *graphics-buffer* (incf i) (& x 0x3f)))))
  (draw-screen))

(defun rect-test ()
  "Тестирование рисования полого прямоугольника"
  (init-screen *screen-width* *screen-height*)
  (draw-rect 30 10 200 100 1)
  (draw-screen))

(defun rectf-test ()
  "Тестирование рисования заполненного прямоугольника"
  (init-screen *screen-width* *screen-height*)
  (draw-rectf 30 10 200 100 1)
  (draw-screen))

(defun line-test ()
  "Тестирование рисования прямой линии"
  (init-screen *screen-width* *screen-height*)
  (draw-line 37 17 176 200 1)
  (draw-line 200 13 20 100 3)
  (draw-line 72 126 13 200 2)
  (draw-screen))

(defun circle-test ()
  "Тестирование рисования круга"
  (init-screen *screen-width* *screen-height*)
  (draw-circle 100 100 20 1)
  (draw-screen))

(defun bezier-test ()
  "Тестирование рисования кривой Безье"
  (init-screen *screen-width* *screen-height*)
  (draw-bezier-curve 10 10 100 100 200 100 300 10 1)
  (draw-screen))

(defun test-part-screen (x y w h c)
  "Тестирование вывода части экрана"
  (let ((i (+ (* y *screen-width*) x -1)))
    (for yy y (+ y h)
	 (for xx x (+ x w)
	      (seta *graphics-buffer* (incf i) c))
	 (setq i (- (+ i *screen-width*) w)))
  (send-graphics-buffer *graphics-buffer* x y w h)))

(init-screen 320 200)
(draw-screen-test)
(clear-screen)
(test-part-screen 30 10 100 50 1)
(test-part-screen 70 30 120 70 2)
(test-part-screen 148 148 1 1 3)
