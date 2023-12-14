; Символы
(defvar +ul-char+ "\xc9") ; левый верхний двойной угол
(defvar +ur-char+ "\xbb") ; правый верхний двойной угол
(defvar +bl-char+ "\xc8") ; левый нижний двойной угол
(defvar +br-char+ "\xbc") ; правый нижний двойной угол
(defvar +h-char+ "\xcd") ; горизонтальная двойная черта
(defvar +v-char+ "\xba") ; вертикальная двойная черта
; Цвета
(defvar +black+ 0)
(defvar +blue+ 1)
(defvar +green+ 2)
(defvar +cyan+ 3)
(defvar +red+ 4)
(defvar +magenta+ 5)
(defvar +brown+ 6)
(defvar +light-gray+ 7)
(defvar +dark-gray+ 8)
(defvar +light-blue+ 9)
(defvar +light-green+ 10)
(defvar +light-cyan+ 11)
(defvar +light-red+ 12)
(defvar +light-magenta+ 13)
(defvar +yellow+ 14)
(defvar +white+ 15)


(defun fill-row (count char)
  "Печатает строку символов char длиной count"
  (if (equal count 0) nil
    (progn
      (putchar char)
      (fill-row (- count 1) char))))

(defun fill-column (x y count char)
  "Печатает вертикальную линию из символов char длиной count, начиная с позиции x,y"
  (if (equal count 0) nil
    (progn
      (set-cursor x y)
      (putchar char)
      (fill-column x (+ y 1) (- count 1) char)))) 
  

(defun fill-rect (x y w h char)
    "Заполняет область символом char"
    "x,y - координаты левого верхнего угла"
    "w - ширина области, h - высота области"
  (if (equal h 0) nil
    (progn
      (set-cursor x y)
      (fill-row w char)
      (fill-rect x (+ y 1) w (- h 1) char))))


(defun print-rect (x y w h)
  "Отрисовывет рамку окна"
  "x,y - координаты левого верхнего угла"
  "w - ширина рамки, h - высота рамки"
  (set-cursor x y)
  (putchar +ul-char+)
  (fill-row (- w 2) +h-char+)
  (putchar +ur-char+)
  (fill-column x (+ y 1) (- h 2) +v-char+)
  (fill-column (+ x (- w 1)) (+ y 1) (- h 2) +v-char+)
  (set-cursor x (+ y (- h 1)))
  (putchar +bl-char+)
  (fill-row (- w 2) +h-char+)
  (putchar +br-char+))

(defclass element ()
  (x ; Координата x (относительно левого верхнего угла родительского элемента)
   y ; Координата y (относительно левого верхнего угла родительского элемента)
   width ; Ширина элемента
   height ; Высота элемента
   text ; Текст элемента
   color ; Цвет текста элемента
   back-color ; Цвет фона элемента
   active-color ; Цвет фона, когда элемент активен
   parent ; Родительский элемент
   children ; Список дочерних элементов
   max-children-bottom ; Максимальное расстояние от верхней границы родительского элемента до нижней границы дочерних элементов
   current-element-pos)) ; Позиция для добавления нового дочернего элемента, например (22 . 5)

(defmacro gen/calc-coord (name coord)
  `(defun ,name (elem)
     "Вычисление абсолютной координаты coord элемента elem"
     (if (null elem) 0
       (+ (slot elem ',coord) (,name (slot elem 'parent))))))

(gen/calc-coord calc-x x)
(gen/calc-coord calc-y y)

(defmethod draw ((self element))
  "Отрисовка элемента"
  (set-color (slot self 'color))
  (set-back-color (slot self 'back-color))
  (fill-rect (slot self 'x)
	     (slot self 'y)
	     (slot self 'width)
	     (slot self 'height)
	     " ")
  (app 'draw (slot self 'children)))

(defmethod add-child ((self element) child)
  "Добавление дочернего элемента child"
  (setf (slot self 'children) (append (slot self 'children) (list child)))
  (setf (slot child 'parent) 'self)
  nil)
