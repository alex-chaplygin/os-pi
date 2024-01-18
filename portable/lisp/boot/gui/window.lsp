(defvar *screen-width* 80); ширина экрана					
;Символы
(defvar +lend-char+ "\xb5") ; левое закрытие перед текстом заголовка
(defvar +rend-char+ "\xc6") ; правое закрытие после текста заголовка

(defclass window element ())

(defmethod set-defaults ((self window))
  (super set-defaults self)
  (setf (slot self 'back-color) +light-blue+)
  (setf (slot self 'height) 2)
  (setf (slot self 'width) 4)
  (set-padding self #(1 1 1 1)))
  
(defmethod draw ((self window))
  (super draw self)
  (set-color (slot self 'color))
  (set-back-color (slot self 'back-color))
  (let ((x (slot self 'x))
	(y (slot self 'y))
	(width (slot self 'width))
	(height (slot self 'height))
	(text (slot self 'text)))
    (print-rect x y width height)
    (set-cursor (+ x (/ (- width (string-size text)) 2) -1)  y)
    (putchar +lend-char+)
    (putstring text)
    (putchar +rend-char+)))
