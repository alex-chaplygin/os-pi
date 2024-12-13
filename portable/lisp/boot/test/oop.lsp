(defun oop-test ()
  (defclass point () (x y)) 
  (defclass line point (x2 y2))
  (setq p2 (make-point 10 20)) ; x = 10 y = 20
  (point-set-x p2 50) ; x = 50 y = 20
  (setq l1 (make-line 1 1 3 3))

  (defmethod move ((self point) dx dy)
    (point-set-x self (+ (point-x self) dx))
    (point-set-y self (+ (point-y self) dy)))

  (defmethod move ((self line) dx dy)
    (line-set-x self (+ (line-x self) dx))
    (line-set-y self (+ (line-y self) dy))
    (line-set-x2 self (+ (line-x2 self) dx))
    (line-set-y2 self (+ (line-y2 self) dy)))
  
  (move p2 20 20) 
  (print (assert (slot p2 'x) 70))
  (print (assert (slot p2 'y) 40))
  (move l1 20 20)
  (print (assert (slot l1 'x) 21))
  (print (assert (slot l1 'y) 21))
  (print (assert (slot l1 'x2) 23))
  (print (assert (slot l1 'y2) 23))
  (point-set-x p2 10)
  (print (point-x p2)))

(oop-test)
