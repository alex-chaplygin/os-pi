(defclass mob () (pos float-pos speed accel))

(defun new-mob (pos)
  "Создать мобильный объект с заданной позицией"
  (make-mob
   pos
   pos
   (make-vec2 0.0 0.0)
   (make-vec2 0.0 0.0)))

(defun move-mob (mob)
  "Передвинуть мобильный объект на новую позицию"
  (mob-set-float-pos mob (vec2-add
			  (vec2-add
			   (mob-float-pos mob)
			   (mob-speed mob))
			  (vec2-scale (mob-accel mob) 0.5)))
  
  (mob-set-speed mob (vec2-add (mob-speed mob) (mob-accel mob)))
  (let* ((pos (mob-float-pos mob))
	 (x (round (vec2-x pos)))
	 (y (round (vec2-y pos))))
    (mob-set-pos mob (cons x y))))

