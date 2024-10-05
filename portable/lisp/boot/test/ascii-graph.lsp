;Модель уровня
(defvar *level* #(#(WALL PLAT WALL PLAT)
		  #(nil WALL nil)))

(defvar *tile-symbols* '((WALL . "#")
			 (PLAT . "-")
			 (NIL . " ")
			 (PLAYER . "@")
			 (PLAYER2 . "&")))

(hide-cursor)
(set-screen *level*)
(set-tiles *tile-symbols*)
(set-tile-size 6 3)
;(putchar (get-hash *tile-hash* 'WALL))
;(draw-tile 'WALL 1 1)
(set-default-sprite-size 10 5)
(setq player-spr (new-sprite 'PLAYER 15 6 0))
(set-sprite-pos player-spr 20 10 3)
(set-sprite-tile player-spr 'PLAYER2)
(set-sprite-size player-spr 1 1)
(draw-screen)

