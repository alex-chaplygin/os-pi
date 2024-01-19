; текстовый элемент
(defclass text element ())

(defmethod set-defaults ((self text))
  (super set-defaults self)
  (setf (slot self 'color) +black+)
  (setf (slot self 'back-color) +white+)
  (setf (slot self 'height) 1)
  (setf (slot self 'width) (string-size (slot self 'text)))
  (set-padding self #(0 0 0 0)))

(defmethod draw ((self text))
  (super draw self)
  (let ((x (slot self 'x))
	(y (slot self 'y))
	(text (slot self 'text)))
    (set-cursor x y)
    (putstring text)))
