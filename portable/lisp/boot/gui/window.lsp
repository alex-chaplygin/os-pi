(defclass window element ())

(defmethod set-defaults ((self window))
  (super set-defaults self)
  (setf (slot self 'back-color) +blue+))
  
