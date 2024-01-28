; текстовый элемент
(defclass text element ())

(defmethod set-defaults ((self text))
  (super set-defaults self)
  (setf (slot self 'color) +black+)
  (setf (slot self 'back-color) +blue+)
  (setf (slot self 'height) 1)
  (setf (slot self 'width) (string-size (slot self 'text)))
  (set-padding self #(0 0 0 0)))

(defmethod draw ((self text))
  (super draw self)
  (let ((x (slot self 'x))
	(y (slot self 'y))
	(txt (slot self 'text)))
    (print-text txt x y (slot self 'width))))

(defun find-last-space (text ind)
  (cond ((equal ind 0) 0)
	(t (if (equal (char text ind) 32) ind
	     (find-last-space text (-- ind))))))
		    
(defun print-text-beta (text x y w)
  "ПОКА НЕ РАБОТЕТ ИЩИТЕ БАГИ Переносит текст на новую строку, если он выходит за пределы поля"
  "text - изначальный текст"
  "x,y - координаты левого верхнего угла"
  "w - ширина элемента"
  (let ((len (string-size text))
	(str (subseq text 0 w))
	(sym (char text (+ w 1)))
	(lasttext))
    (if (= sym 32)
	(progn
	  (set-cursor x y)
	  (putstring str)
	  (lasttext (subseq text w len)))
      (progn
	(lastind (find-last-space str))
	(set-cursor x y)
	(putstring (subseq str 0 lastind))
	(lasttext  (subseq text lastind len))))
        
    (when (> len w)
	(print-text lasttext x (+ y 1) w))))
    
        
 (defun print-text (txt x y w)
  "text - изначальный текст"
  "x,y - координаты левого верхнего угла"
  "w - ширина элемента"
  (let* ((len (string-size txt))
	; (sym (char txt (+ w 1)))
	 (str (subseq txt 0 (if (> w len) len w))))
	 ;(idx w));(if (equal sym 32) (find-last-space str (- len 1)) w)))
    (set-cursor x y)
    (putstring str)
    (when (> len w)
      (print-text (subseq txt w len) x (+ y 1) w))))
