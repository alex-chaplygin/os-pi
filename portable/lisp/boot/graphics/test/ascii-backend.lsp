(defun test-all-screen ()
  "Заполнить весь экран разноцветными символами"
  (let ((i 0)) 
    (for row 0 +text-height+
	 (for col 0 +text-width+
	      (seta *text-buffer* i col)
	      (incf i)
	      (seta *text-buffer* i col)
	      (incf i))))
  (send-text-buffer *text-buffer* 0 0 +text-width+ +text-height+))

(defun test-part-screen (x y w h)
  "Заполнить часть экрана с координатами x, y и шириной w, высотой h"
  (for col x +text-width+
       (for row y +text-height+
	    (let ((i (+ (* +text-width+ row 2) (* col 2))))
	      (seta *text-buffer* i row)
	      (seta *text-buffer* (++ i) row))))
  (send-text-buffer *text-buffer* x y w h))


(test-all-screen)
(clear-screen)
(test-part-screen 20 10 20 10)
(test-part-screen 30 15 30 15)
(test-part-screen 61 5 1 1)
