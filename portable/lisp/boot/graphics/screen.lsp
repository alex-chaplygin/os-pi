;; Вычисление области экрана, которую нужно перерисовать
(defvar *screen-width*) ; ширина экрана
(defvar *screen-height*) ; высота экрана
(defvar *draw-top-left*) ;Координаты левого верхнего угла перерисовываемой области
(defvar *draw-bottom-right*) ;Координаты правого нижнего угла перерисовываемой области
(defvar *graphics-buffer*) ; внутренний буфер пикселей
  
(defun screen-reset ()
  "Сбросить координаты области перерисовки"
  (setq *draw-bottom-right* (make-vec2 0 0))
  (setq *draw-top-left* (make-vec2 *screen-width* *screen-height*)))

(defun add-to-redraw-rect (x1 y1 x2 y2)
  "Вспомогательная функция добавления прямоугольника в область отрисовки"
  (when (< x1 (vec2-x *draw-top-left*)) (setq *draw-top-left* (cons x1 (cdr *draw-top-left*))))
  (when (< y1 (vec2-y *draw-top-left*)) (setq *draw-top-left* (cons (car *draw-top-left*) y1)))
  (when (> x2 (vec2-x *draw-bottom-right*)) (setq *draw-bottom-right* (cons x2 (cdr *draw-bottom-right*))))
  (when (> y2 (vec2-y *draw-bottom-right*)) (setq *draw-bottom-right* (cons (car *draw-bottom-right*) y2))))

(defun screen-add-rect (ul size)
  "Добавить прямоугольник в область отрисовки"
  "ul - координаты верхнего левого угла"
  "size - вектор размеров"
  (let* ((x1 (vec2-x ul))
	 (y1 (vec2-y ul))
	 (x2 (+ x1 (vec2-x size)))
	 (y2 (+ y1 (vec2-y size))))
    (add-to-redraw-rect x1 y1 x2 y2)))

(defun screen-add-point (p)
  "Добавить точку в область отрисовки"
  (let* ((x (car p))
	 (y (cdr p)))
    (add-to-redraw-rect x y x y)))
