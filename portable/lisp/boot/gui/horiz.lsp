(defclass horiz block (padding curpos sizeable))

(defmethod add-child ((self horiz) ch)
  (element-set-x ch (horiz-curpos self))
  (element-set-y ch 0)
  (element-set-parent ch self)
  (let ((x (+ (horiz-curpos self) (horiz-padding self) (element-width ch)))
	(y (element-height ch)))
    (when (horiz-sizeable self)
      (when (> y (horiz-height self))
        (horiz-set-height self y))
      (when (> x (horiz-width self))
        (horiz-set-width self x)))
    (horiz-set-curpos self x))
  (horiz-set-children self (append (horiz-children self) (list ch))))

(defmethod set-defaults ((self horiz))
  "установка значений по умолчанию"
  (super set-defaults self)
  (horiz-set-padding self 1)
  (horiz-set-curpos self 0)
  (horiz-set-sizeable self T))
  
