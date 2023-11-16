(defvar +screen-width+ 320)
(defvar +screen-height+ 200)
(defvar +screen-depth+ 8)
(defvar *gr-buf* (make-array (* +screen-width+ +screen-height+ (/ +screen-depth+ 8))))

(defun gr-test ()
  ;(bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (for y 0 +screen-height+
       (progn
	 (for x 0 +screen-width+
	      (seta *gr-buf* (+ x (* y +screen-width+)) (& x 0xff)))))
  (graph-send-buffer *gr-buf*))

;(gr-test)
;(bgr-set-res 1024 768 8)
