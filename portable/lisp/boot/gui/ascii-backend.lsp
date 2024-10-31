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

(defun fill-rect (x y w h)
    "Заполняет область пробелом"
    "x,y - координаты левого верхнего угла"
    "w - ширина области, h - высота области"
    (let ((ww (if (> (+ x w) *screen-width*)
		  (- *screen-width* x) w))
	  (hh (if (> (+ y h) *screen-height*)
		  (- *screen-height* y) h))
	  (c (get-hash *cur-state* 'color)))
      (set-color c)
      (set-back-color c)
      (for i y (+ y hh)
	 (set-cursor x i)
	 (fill-row ww " "))))

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
