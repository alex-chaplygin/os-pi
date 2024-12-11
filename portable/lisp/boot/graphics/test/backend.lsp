(init-screen 320 200)
(defun draw-screen-test ()
  "Тестирование вывода экрана"
  (let ((i -1))
    (for y 0 *screen-height*
      (for x 0 *screen-width*
                (seta *graphics-buffer* (incf i) (& x 0x3f)))))
  (draw-screen))

(defun test-part-screen (x y w h c)
  "Тестирование вывода части экрана"
  (clear-screen)
  (let ((i (+ (* y *screen-width*) x -1)))
    (for yy y (+ y h)
	 (for xx x (+ x w)
	      (seta *graphics-buffer* (incf i) c))
	 (setq i (- (+ i *screen-width*) w)))
  (send-graphics-buffer *graphics-buffer* x y w h)))

(defun rect-test ()
  "Тестирование рисования полого прямоугольника"
  (clear-screen)
  (draw-rect 30 10 200 100 1)
  (draw-screen))

(defun rectf-test ()
  "Тестирование рисования заполненного прямоугольника"
  (clear-screen)
  (draw-rectf 30 10 200 100 1)
  (draw-screen))

(defun line-test ()
  "Тестирование рисования прямой линии"
  (clear-screen)
  (draw-line 37 17 176 200 1)
  (draw-line 200 13 20 100 3)
  (draw-line 72 126 13 200 2)
  (draw-screen))

(defun circle-test ()
  "Тестирование рисования круга"
  (clear-screen)
  (draw-circle 100 100 20 1)
  (draw-screen))

(defun bezier-test ()
  "Тестирование рисования кривой Безье"
  (clear-screen)
  (draw-bezier-curve '(0 . 0) '(10 . 10) '(20 . 10) '(30 . 10) 10 1)
  (draw-bezier-curve '(20 . 120) '(30 . 100) '(140 . 100) '(150 . 120) 20 2)
  (draw-bezier-curve '(160 . 100) '(170 . 100) '(300 . 100) '(319 . 100) 20 3)
  (draw-screen))

(defun test-set-pixel ()
  "Тестирование установки пикселя"
  (clear-screen)
  (set-pixel 10 10 1)
  (set-pixel 20 20 2)
  (set-pixel 30 30 3)
  (draw-screen))

(defun test-fill-triangle ()
  "Тестирование заливки треугольника"
  (clear-screen)
  (fill-triangle '(10 . 1) '(1 . 8) '(20 . 20) 2)
  (draw-screen))

(defun test-draw-hline ()
  "Тестирование рисования горизонтальной линии"
  (clear-screen)
  (draw-hline 50 100 80 3)
  (draw-screen))

(defun test-draw-image ()
  "Тестирование вывода изображения"
  (let ((width 10)
        (height 10)
        (image (make-image #(#(0 1 1 0)
			     #(2 2 2 2)
			     #(2 3 3 2)
			     #(0 3 3 0)))))
    
    (clear-screen)
    (draw-image image)
    (draw-screen)))

;(draw-screen-test)
;(clear-screen)
;(test-part-screen 30 10 100 50 1)
;(test-part-screen 70 30 120 70 2)
;(test-part-screen 148 148 1 1 3)

;-(rect-test)
;(rectf-test)
;-(line-test)
;(circle-test)
(bezier-test)
;(test-set-pixel)
;(test-fill-triangle)
;(test-draw-hline)
;-(test-draw-image)
