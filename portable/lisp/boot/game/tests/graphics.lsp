(defvar p (char-code #\#))
(defvar im (make-image `#(#(,p ,p)
			  #(,p ,p))))
(defvar spr-im (make-image `#(#(49 49)
			      #(49 49))))
(defvar empty (make-image #(#(0 0)
			    #(0 0))))
(defvar *background* #(#(WALL nil nil WALL)
		       #(WALL nil nil WALL)
		       #(WALL WALL WALL WALL)))
(defvar *tiles-dictionary* `((WALL . ,im)
			     (nil . ,empty)))

(set-colour +light-gray+)
(set-tiles *tiles-dictionary*)
(set-tile-size 2 2)
(set-background *background*)
(setq spr (new-sprite spr-im (make-vec2 5 5) (make-vec2 2 2)))
(draw-screen)
(set-sprite-pos spr (make-vec2 0 0))
(draw-screen)
