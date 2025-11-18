(defclass block element (children))

(defmethod add-child ((self block) ch)
  "Добавляет элемент ch в список children обьекта block"
  (block-set-children self (append (block-children self) (list ch))))

(defmethod draw ((self block))
  "Вызывает отрисовку всех дочерних элементов блока"
  (gsave)
  (translate (cons (block-x self) (block-y self)))
  (set-colour (block-back-colour self))
  (fill-rect 0 0 (block-width self) (block-height self))
  (app #'draw (block-children self))
  (grestore))
