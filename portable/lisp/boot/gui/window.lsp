(defclass window element ())

(defmethod set-defaults ((self element))
  (setf (slot self 'color) +black+)
  (setf (slot self 'back-color) +light-gray+)
  (setf (slot self 'active-color) +white+))

(defmethod set-defaults ((self window))
  (super set-defaults self)
  (setf (slot self 'back-color) +blue+))
  
