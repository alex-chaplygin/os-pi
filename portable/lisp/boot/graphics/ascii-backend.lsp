; Цвета
(defconst +black+ 0)
(defconst +blue+ 1)
(defconst +green+ 2)
(defconst +cyan+ 3)
(defconst +red+ 4)
(defconst +magenta+ 5)
(defconst +brown+ 6)
(defconst +light-gray+ 7)
(defconst +dark-gray+ 8)
(defconst +light-blue+ 9)
(defconst +light-green+ 10)
(defconst +light-cyan+ 11)
(defconst +light-red+ 12)
(defconst +light-magenta+ 13)
(defconst +yellow+ 14)
(defconst +white+ 15)
(defvar *screen-width* 80) ; число колонок экрана
(defvar *screen-height* 25) ; число строк экрана
(defconst +buffer-size+ (* (* *screen-width* *screen-height*) 2)) ; размер буфера экрана
(defvar *text-buffer* (make-array +buffer-size+)) ; буфер для текстовой видеопамяти

(defun clear-screen()
  "Очистка экрана"
  (hide-cursor)
  (for i 0 +buffer-size+
       (seta *text-buffer* i 0))
  (send-text-buffer *text-buffer* 0 0 *screen-width* *screen-height*))

(defun fill-rect (x y w h)
  "Заполняет область пробелом с учетом матрицы преобразования"
  (let* ((c (get-hash *cur-state* 'color))
         (x2 (+ x w))
         (y2 (+ y h))
         (top-left (mat-mul-vec ctm (cons x y)))
         (bottom-right (mat-mul-vec ctm (cons x2 y2)))
         (tx1 (car top-left))
         (ty1 (cdr top-left))
         (tx2 (car bottom-right))
         (ty2 (cdr bottom-right))
         (new-w (- tx2 tx1))
         (new-h (- ty2 ty1)))
    (set-color c)
    (set-back-color c)
    (for i ty1 (+ ty1 new-h)
      (set-cursor tx1 i)
      (fill-row new-w #\ ))))

(defun show-text (text)
  "Вывод текста в текущей позиции"
  (let* ((ctm (get-hash *cur-state* 'ctm))
         (p (mat-mul-vec ctm *cur-point*))
         (c (get-hash *cur-state* 'color))
         (ofs (<< (+ (* (cdr p) *screen-width*) (car p)) 1)))
    (for i 0 (string-size text)
         (setf (aref *text-buffer* ofs) (char-code (char text i)))
         (incf ofs)
         (setf (aref *text-buffer* ofs) c)
         (incf ofs))))

(defun draw-image (image)
  "Вывод изображения в позиции матрицы трансформации"
  (let* ((ctm (get-hash *cur-state* 'ctm))
	 (color (get-hash *cur-state* 'color))
	 (p (mat-mul-vec ctm '(0 . 0)))
	 (ofs (<< (+ (* (cdr p) *screen-width*) (car p)) 1))
	 (width (image-width image))
	 (height (image-height image)))
    (screen-add-rect p (cons width height))
    (for y 0 height
	 (let ((row (image-row image y)))
	   (for x 0 width
		(seta *text-buffer* ofs (aref row x))
		(incf ofs)
		(seta *text-buffer* ofs color)
		(incf ofs))
	   (setq ofs (+ ofs (<< *screen-width* 1) (- 0 (<< width 1))))))))

(defun draw-screen ()
  "Перерисовка экрана"
  (let*  ((x (vec2-x *draw-top-left*))
	  (y (vec2-y *draw-top-left*))
	  (width (- (vec2-x *draw-bottom-right*) x))
	  (height (- (vec2-y *draw-bottom-right*) y)))
    (when (and (> width 0) (> height 0))
      (send-text-buffer *text-buffer* x y width height)))
  (screen-reset))

(screen-reset)
