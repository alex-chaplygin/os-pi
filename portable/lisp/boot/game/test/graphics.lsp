(defvar p (char-code #\#))
(defvar im (make-image `#(#(,p ,p)
			  #(,p ,p))))

(defvar spr-im1 (make-image `#(#(49 49)
			      #(49 49))))

(defvar spr-im2 (make-image `#(#(43 43)
			       #(43 43))))

(defvar spr-im3 (make-image `#(#(36 36)
			       #(36 36))))

(defvar spr-im4 (make-image `#(#(60 62)
			       #(94 94))))

(defvar empty (make-image #(#(0 0)
			    #(0 0))))

(defvar *background* #(#(WALL WALL WALL WALL)
		       #(WALL WALL WALL WALL)
		       #(WALL WALL WALL WALL)))

(defvar *tiles-dictionary* `((WALL . ,im)
			     (nil . ,empty)))

(defun get-layers ()
  "Получить слой каждого спрайта из списка спрайтов"
  (map #'(lambda (spr) (sprite-layer spr)) *sprites*))

(clear-screen)
(set-colour +light-gray+)
(set-tiles *tiles-dictionary*)
(set-tile-size (make-vec2 2 2))
(set-background *background*)
(setq spr (new-sprite spr-im1 (make-vec2 3 2) 0))
(draw-screen)

					;Если нарисован только один спрайт, то перемещается нормально.
					(set-sprite-pos spr (make-vec2 0 0))
					(draw-screen)

(setq spr2 (new-sprite spr-im2 (make-vec2 2 1) 2))
(draw-screen)
(setq spr3 (new-sprite spr-im3 (make-vec2 4 3) 1))
(draw-screen)
(setq spr4 (new-sprite spr-im4 (make-vec2 1 0) 0))
(draw-screen)
(delete-sprite spr2)
(draw-screen)
(sprite-set-image spr4 spr-im2)
(draw-screen)
					;То же самое и с удалением спрайтов.
					(delete-sprite spr3)
					(draw-screen)

					;Слетает кодировка изображения спрайта при его перемещении, если на экране 4 спрайта.
					(set-sprite-pos spr4 (make-vec2 1 0))

					(draw-screen)
