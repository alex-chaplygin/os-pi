(defclass window element ())

(defmethod set-defaults ((self window))
  (super set-defaults self)
  (setf (slot self 'back-color) +light-blue+)
  (set-padding self #(1 1 1 1)))
  
(defmethod draw ((self window))
  (super draw self)
  (set-color (slot self 'color))
  (set-back-color (slot self 'back-color))
  (print-rect (slot self 'x) (slot self 'y) (slot self 'width) (slot self 'height)))
