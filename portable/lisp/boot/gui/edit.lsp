(defclass edit text ())

(defmethod set-defaults ((self edit))
  (super set-defaults self)
  (setf (slot self 'color) +light-gray+)
  (setf (slot self 'back-color) +light-cyan+))
