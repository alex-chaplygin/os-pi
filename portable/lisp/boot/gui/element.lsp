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
   ofs-x ; Смещение для дочерних элементов
   ofs-y
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

(defmethod draw ((self element))
  "Отрисовка элемента"
  (set-color (slot self 'color))
  (set-back-color (slot self 'back-color))
  (let* ((ofs-x (slot self 'ofs-x))
	 (ofs-y (slot self 'ofs-y))
	 (new-x (+ (slot self 'x) ofs-x))
	 (new-y (+ (slot self 'y) ofs-y))
	 (w (slot self 'width))
	 (h (slot self 'height)))
    (fill-rect new-x new-y w h " ")
    (app '(lambda (el)
	   (setf (slot self 'ofs-x) new-x) ;устанавливаем новое смещение для дочерних элементов
	   (setf (slot self 'ofs-y) new-y)
	   (draw el)
	   (setf (slot self 'ofs-x) ofs-x) ;восстанавливаем смещение
	   (setf (slot self 'ofs-y) ofs-y)) (slot self 'children))))

(defmethod add-child ((self element) child)
  "Добавление дочернего элемента child"
  (setf (slot self 'children) (append (slot self 'children) (list child)))
  (setf (slot child 'parent) self)
  (let* ((cur-x (car (slot self 'current-element-pos))) ;Позиция куда добавляется элемент
	 (cur-y (cdr (slot self 'current-element-pos)))
	 (new-cur-x (+ cur-x (slot child 'width))) ;Новая позиция для добавления
	 (total-width (+ new-cur-x (aref (slot self 'padding) 1)))
	 (height (+ (slot child 'height) (aref (slot self 'padding) 3))))
    (update-width self total-width)
    (update-height self height)
    (setf (slot child 'x) cur-x)
    (setf (slot child 'y) cur-y)
    (setf (slot self 'current-element-pos) (cons new-cur-x cur-y))) 
  nil)
     
(defmacro mk/update (name coord param pad)
  "Макрос, создающий функцию name для обновления ширины/высоты"
  "которая выбирается как координата coord и параметр param"
  "используя смещение pad в массиве padding элемента"
  `(defun ,name (elem ,param)
     (unless (null elem)
       (let ((,coord (slot elem ',coord))
	     (siz (slot elem ',param))
	     (parent (slot elem 'parent)))
	 (when (> ,param siz) (setf (slot elem ',param) ,param))
	 (unless (null parent)
	   (,name  parent (+ ,param ,coord (aref (slot parent 'padding) ,pad))))))))

(mk/update update-width x width 1)
(mk/update update-height y height 3)

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
  (setf (slot self 'x) 0)
  (setf (slot self 'y) 0)
  (setf (slot self 'ofs-x) 0) ; смещение для отрисовки дочерних элементов
  (setf (slot self 'ofs-y) 0)
  (setf (slot self 'current-element-pos) (cons 0 0))
  (setf (slot self 'max-children-bottom) 0))

(defmethod set-padding ((self element) padding)
  (setf (slot self 'current-element-pos)
	(cons (aref padding 0) (aref padding 2)))
  (setf (slot self 'padding) padding))
  
  
