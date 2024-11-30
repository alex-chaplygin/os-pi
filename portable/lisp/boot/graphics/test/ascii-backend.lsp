(defun test-all-screen ()
  "Заполнить весь экран разноцветными символами"
  (for col 0 +text-width+
       (for row 0 +text-height+
	    (let ((i (+ (* +text-width+ row 2) (* col 2))))
	      (seta *text-buffer* i col)
	      (seta *text-buffer* (++ i) col))))
  (send-text-buffer *text-buffer* 0 0 +text-width+ +text-height+))

(defun test-part-screen ()
  "Заполнить весь экран разноцветными символами"
  (for col 20 +text-width+
       (for row 10 +text-height+
	    (let ((i (+ (* +text-width+ row 2) (* col 2))))
	      (seta *text-buffer* i row)
	      (seta *text-buffer* (++ i) row))))
  (send-text-buffer *text-buffer* 20 10 20 10))


(test-all-screen)
(test-part-screen)
