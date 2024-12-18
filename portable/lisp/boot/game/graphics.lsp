(defvar *background*)
(defvar *tile-w* 1)
(defvar *tile-h* 1)
(defvar *tile-hash*)
(defclass Sprite () (image x y width height))

(defun set-background (matrix)
  "Задать фон двумерным массивом имен изображений"
  "set-background #(#(WALL PLAT WALL PLAT)
		    #(nil WALL nil))"
  (setq *background* matrix)
  (draw-background))

(defun set-tile-size (w h)
  "Установить размеры тайла"
  "set-tile-size 20 10"
  (setq *tile-w* w)
  (setq *tile-h* h))

(defun set-tiles (tiles)
  "Задать список тайлов"
  "set-tiles `((WALL . ,image1) (PLAT . ,image2))"
  (setq *tile-hash* (make-hash))
  (app #'(lambda (tile) (set-hash *tile-hash* (car tile) (cdr tile))) tiles))

(defun new-sprite (image pos size)
  "Создать новый спрайт с заданным изображением и координатами"
  (let ((sprite (make-Sprite
		 image
		 (car pos)
		 (cdr pos)
		 (car size)
		 (cdr size))))
    (draw-sprite sprite)
    sprite))

(defun set-sprite-pos (sprite pos)
  "Установить позицию переданного спрайта"
  (delete-sprite sprite)
  (setf (slot sprite 'x) (car pos))
  (setf (slot sprite 'y) (cdr pos))
  (draw-sprite sprite))

(defun set-sprite-image (sprite image)
  "Установить изображение для переданного спрайта"
  (setf (slot sprite 'tile) image))

(defun delete-sprite (sprite)
  "Удалить переданный спрайт с экрана"
  (draw-part-background (slot sprite 'x) (slot sprite 'y) (slot sprite 'width) (slot sprite 'height)))

;--------------------------------------сверху API-----------------------------
(defun draw-sprite (sprite)
  "Отрисовать переданный спрайт"
  (gsave)
  (translate (cons (slot sprite 'x) (slot sprite 'y)))
  (draw-image (slot sprite 'image))
  (grestore))
  
(defun draw-background ()
  "Отрисовать сетку тайлов"
  (gsave)
  (for r 0 (array-size *background*)
       (let* ((row (aref *background* r))
	      (rowsize (array-size row)))
	 (for c 0 rowsize
	      (draw-image (get-hash *tile-hash* (aref row c)))
	      (translate (cons *tile-w* 0)))
	      (translate (cons (* rowsize (- 0 *tile-w*)) *tile-h*))))
  (grestore))

(defun get-tile-index (x y)
  "Получить индекс тайла по координатам"
  (cons (/ y *tile-h*) (/ x *tile-w*)))

(defun draw-part-background (x y width height)
  "Отрисовать фон в заданном регионе"
  (gsave)
  (let* ((start-tile-ind (get-tile-index x y))
       	 (end-tile-ind (get-tile-index (+ x width) (+ y height)))
	 (start-row (car start-tile-ind))
	 (start-column (cdr start-tile-ind))
	 (end-row (+ 1 (car end-tile-ind)))
	 (end-column (+ 1 (cdr end-tile-ind))))
    (translate (cons (* start-column *tile-w*) (* start-row *tile-h*)))
    (for row-ind start-row end-row
	 (let ((row (aref *background* row-ind)))
	   (for column-ind start-column end-column
		(draw-image (get-hash *tile-hash* (aref row column-ind)))
		(translate (cons *tile-w* 0))))
	 (translate (cons (* (- start-column end-column) *tile-w*) *tile-h*))))
  (grestore))
