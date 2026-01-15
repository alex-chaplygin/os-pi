(defclass element ()
  (id ; уникальный идентификатор элемента
   x ; Координата x (относительно левого верхнего угла родительского элемента)
   y ; Координата y (относительно левого верхнего угла родительского элемента)
   width ; Ширина элемента
   height ; Высота элемента
   back-colour ; Цвет фона элемента
   active-colour ; Цвет фона, когда элемент активен
   parent ; Родительский элемент
   children ; Список дочерних элементов
   keyup ; Функция обработки нажатия клавиши
   keydown)) ; Функция обработки отжатия клавиши


(defmethod draw ((self element))
  "Отрисовка элемента"
  (set-colour (element-back-colour self))
  (unless (element-width self) (element-set-width self 1))
  (unless (element-height self) (element-set-height self 1))
  (fill-rect (element-x self) (element-y self) (element-width self) (element-height self)))

(defmethod set-defaults ((self element))
  "установка значений по умолчанию"
  (element-set-back-colour self +yellow+)
  (element-set-active-colour self +red+)
  (element-set-x self 0)
  (element-set-y self 0))
  
