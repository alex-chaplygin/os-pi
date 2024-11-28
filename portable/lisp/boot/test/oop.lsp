(defun oop-test ()
  (defclass point () (x y)) 
  (defclass line point (x2 y2))
  (setq p2 (make-point 10 20)) ; x = 10 y = 20
  (setf (slot p2 'x) 50) ; x = 50 y = 20
  (setq l1 (make-line 1 1 3 3))

  (defmethod move ((self point) dx dy)
    (setf (slot self 'x) (+ (slot self 'x) dx))
    (setf (slot self 'y) (+ (slot self 'y) dy)))

  (defmethod move ((self line) dx dy)
    (setf (slot self 'x) (+ (slot self 'x) dx))
    (setf (slot self 'x2) (+ (slot self 'x2) dx))
    (setf (slot self 'y) (+ (slot self 'y) dy))
    (setf (slot self 'y2) (+ (slot self 'y2) dy)))
  (move p2 20 20) 
  (print (assert (slot p2 'x) 70))
  (print (assert (slot p2 'y) 40))
  (move l1 20 20)
  (print (assert (slot l1 'x) 21))
  (print (assert (slot l1 'y) 21))
  (print (assert (slot l1 'x2) 23))
  (print (assert (slot l1 'y2) 23)))

(oop-test)
