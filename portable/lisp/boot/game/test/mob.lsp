(defun test-new-mob ()
  (let ((mob (new-mob (make-vec2 10 -20)))
	(mobf (new-mob (make-vec2 -5.5 2.3))))
    (print (assert (mob-pos mob) '(10 . -20)))
    (print (assert (mob-pos mobf) '(-5.5 . 2.3)))))
    
(defun test-move-mob ()
  (let ((mob (new-mob (make-vec2 0 0))))
    (mob-set-accel mob (make-vec2 0 -0.5))
    (move-mob mob)
    (move-mob mob)
    (move-mob mob)
    (print (assert (mob-speed mob) '(0.0 . -1.5)))
    (print (assert (mob-float-pos mob) '(0.0 . -2.25)))
    (print (assert (mob-pos mob) '(0 . -2)))
    (mob-set-accel mob (make-vec2 5 2))
    (move-mob mob)
    (move-mob mob)
    (print (assert (mob-float-pos mob) '(10.0 . -1.25)))
    (print (assert (mob-pos mob) '(10 . -1)))))

(test-new-mob)
(test-move-mob)
