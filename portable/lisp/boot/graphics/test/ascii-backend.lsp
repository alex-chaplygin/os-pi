(defun test-all-screen ()
  "Заполнить весь экран разноцветными символами"
  (let ((i 0)) 
    (for row 0 *screen-height*
	 (for col 0 *screen-width*
	      (seta *text-buffer* i col)
	      (incf i)
	      (seta *text-buffer* i col)
	      (incf i))))
  (send-text-buffer *text-buffer* 0 0 *screen-width* *screen-height*))

(defun test-part-screen (x y w h)
  "Заполнить часть экрана с координатами x, y и шириной w, высотой h"
  (for col x *screen-width*
       (for row y *screen-height*
	    (let ((i (+ (* *screen-width* row 2) (* col 2))))
	      (seta *text-buffer* i row)
	      (seta *text-buffer* (++ i) row))))
  (send-text-buffer *text-buffer* x y w h))


(test-all-screen)
(clear-screen)
(test-part-screen 20 10 20 10)
(test-part-screen 30 15 30 15)
(test-part-screen 61 5 1 1)
(clear-screen)
(defvar p (char-code #\#))
(defvar im (make-image `#(#(,p ,p)
			  #(,p ,p))))
(set-colour +light-gray+)
(translate '(0 . 0))
(draw-image im)
(translate '(10 . 10))
(set-colour +green+)
(draw-image im)
(translate '(0 . -5))
(set-colour +yellow+)
(draw-image im)
(move-to 40 10)
(show-text "Hello")
(set-colour +cyan+)
(translate '(10 . 0))
(fill-rect 0 0 12 5)
(draw-screen)
