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
  (fill-rect (slot self 'x) (slot self 'y) (slot self 'width) (slot self 'height)))

(defmethod set-defaults ((self element))
  "установка значений по умолчанию"
  (element-set-back-colour self +yellow+)
  (element-set-active-colour self +red+)
  (element-set-x self 0)
  (element-set-y self 0)
  (element-set-width self 1)
  (element-set-height self 1))
  
