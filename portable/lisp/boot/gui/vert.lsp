(defclass vert block (padding curpos sizeable))

(defmethod add-child ((self vert) ch)
  (line-set-x ch 0)
  (line-set-y ch curpos)
  (let ((y (+ (line-curpos self) padding (element-height ch)))
	(x (element-width ch)))
    (when sizeable
      (when (> y (vert-height self))
        (line-set-height self y))
      (when (> x (vert-width self))
        (line-set-width self)))
    (line-set-curpos y))
  (line-set-children self (append (vert-children self) (list ch))))
