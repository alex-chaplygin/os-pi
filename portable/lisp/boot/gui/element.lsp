					; Глобальные переменные
(defvar *screen-width* 80); ширина экрана
(defvar *screen-height* 25) ; высота экрана 
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
  (for ii 0 count
      (putchar char)))

(defun fill-column (x y count char)
  "Печатает вертикальную линию из символов char длиной count, начиная с позиции x,y"
  (let ((hh (if (> (+ y count) *screen-height*)
		*screen-height* (+ y count))))
    (for row y hh
	 (set-cursor x row)
	 (putchar char))))

(defun fill-rect (x y w h char)
    "Заполняет область символом char"
    "x,y - координаты левого верхнего угла"
    "w - ширина области, h - высота области"
    (let ((ww (if (> (+ x w) *screen-width*)
		  (- *screen-width* x) w))
	  (hh (if (> (+ y h) *screen-height*)
		  (- *screen-height* y) h)))
      (for i y (+ y hh)
	 (set-cursor x i)
	 (fill-row ww char))))

(defun print-rect (x y w h)
  "Отрисовывет рамку окна"
  "x,y - координаты левого верхнего угла"
  "w - ширина рамки, h - высота рамки"
  (let* ((w-overflow (> (+ x (- w 2)) *screen-width*))
	(ww (if w-overflow
		(- (- *screen-width* x) 2) (- w 2))))
    (set-cursor x y)
    (putchar +ul-char+)
    (fill-row ww +h-char+)
    (putchar (if w-overflow +h-char+ +ur-char+))
    (fill-column x (+ y 1) (- h 2) +v-char+)
    (unless w-overflow
      (fill-column (+ x (+ ww 1)) (+ y 1) (- h 2) +v-char+))
    (set-cursor x (+ y (- h 1)))
    (putchar +bl-char+)
    (fill-row ww +h-char+)
    (putchar +br-char+)))

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
   current-element-pos ; Позиция для добавления нового дочернего элемента, например (22 . 5)
   padding)) ; Массив с информацией об отступах с четырех сторон между рамкой окна и его дочерними элементами

(defmacro gen/calc-coord (name coord)
  `(defun ,name (elem)
     "Вычисление абсолютной координаты coord элемента elem"
     (if (null elem) 0
       (+ (slot elem ',coord) (,name (slot elem 'parent))))))

(gen/calc-coord calc-x x)
(gen/calc-coord calc-y y)

(defmethod draw-offset ((self element) offs-x offs-y)
  "Отрисовка элемента со смещением offs-x offs-y"
  (set-color (slot self 'color))
  (set-back-color (slot self 'back-color))
  (let ((new-x (+ (slot self 'x) offs-x))
	(new-y (+ (slot self 'y) offs-y))
	(w (slot self 'width))
	(h (slot self 'height)))
    (fill-rect new-x new-y w h " ")
    (app '(lambda (el) (draw-offset el new-x new-y)) (slot self 'children))))

(defmethod draw ((self element))
  (draw-offset self 0 0))

(defmethod add-child ((self element) child)
  "Добавление дочернего элемента child"
  (setf (slot self 'children) (append (slot self 'children) (list child)))
  (setf (slot child 'parent) self)
  (let* ((cur-x (car (slot self 'current-element-pos))) ;Позиция куда добавляется элемент
	 (cur-y (cdr (slot self 'current-element-pos)))
	 (new-cur-x (+ cur-x (slot child 'width))) ;Новая позиция для добавления
	 (total-width (+ new-cur-x (aref (slot self 'padding) 1))))
    (update-width self total-width)
    (setf (slot child 'x) cur-x)
    (setf (slot child 'y) cur-y)
    (setf (slot self 'current-element-pos) (cons new-cur-x cur-y))) 
  nil)

(defun update-width (elem width)
  "Изменяет ширину элемента elem на width, если width больше ширины элемента"
  "Повторяет это для родительского элемента"
  (unless (null elem)
    (let ((x (slot elem 'x))
	  (w (slot elem 'width))
	  (parent (slot elem 'parent)))
      (when (> width w) (setf (slot elem 'width) width))
      (unless (null parent)
	(update-width parent (+ width x (aref (slot parent 'padding) 1)))))))

(defmethod align ((self element) name-pos)
  "Выровнять элемент относительно родителя или экрана для окна"
  (let* ((parent (slot self 'parent))
	 (left-x (aref (slot parent 'padding) 0))
	 (pad-right (aref (slot parent 'padding) 1))
	 (parent-width (slot parent 'width))
	 (self-width (slot self 'width)))
    (setf (slot self 'x) 
	  (case name-pos
	    ('left left-x)
	    ('right (- parent-width self-width pad-right))
	    ('center (- (/ parent-width 2) (/ self-width 2)))))))

(defmethod set-defaults ((self element))
  "Устанавливает цвета по умолчанию, позицию для дочерних элементов, максимальное расстояние"
  (setf (slot self 'color) +black+)
  (setf (slot self 'back-color) +light-gray+)
  (setf (slot self 'active-color) +white+)
  (setf (slot self 'current-element-pos) (cons 0 0))
  (setf (slot self 'max-children-bottom) 0))

(defmethod set-padding ((self element) padding)
  (setf (slot self 'current-element-pos)
	(cons (aref padding 0) (aref padding 2)))
  (setf (slot self 'padding) padding))
  
  
