(defvar *background*) ;; Массив имен изображений, находящихся на фоне
(defvar *tile-w* 1) ;; Ширина тайла
(defvar *tile-h* 1) ;; Высота тайла
(defvar *tile-hash*) ;; Хеш таблица: ключ - имя изображения, значение - само изображение
(defvar *sprites*) ;; Список спрайтов
(defclass sprite () (image pos layer))

(defun set-background (matrix)
  "Задать фон двумерным массивом имен изображений"
  "set-background #(#(WALL PLAT WALL PLAT)
		    #(nil WALL nil))"
  (setq *background* matrix)
  (draw-background))

(defun set-tile-size (size)
  "Установить размеры тайла"
  "set-tile-size 20 10"
  (setq *tile-w* (vec2-x size))
  (setq *tile-h* (vec2-y size)))

(defun set-tiles (tiles)
  "Задать список тайлов"
  "set-tiles `((WALL . ,image1) (PLAT . ,image2))"
  (setq *tile-hash* (make-hash))
  (app #'(lambda (tile) (set-hash *tile-hash* (car tile) (cdr tile))) tiles))

(defun new-sprite (image pos &rest layer)
  "Создать новый спрайт с заданным изображением и координатами"
  (if (null layer)
      (setq layer 0)
      (setq layer (car layer)))
  (let ((sprite (make-sprite
		 image
		 pos
		 layer))
	(added-flag nil))
    (setq *sprites* (foldr #'(lambda (spr list)
				 (if (and (>= (sprite-layer spr) layer) (null added-flag))
				     (progn
				       (setq added-flag T)
				       (cons spr (cons sprite list)))
				     (cons spr list)))
			   nil *sprites*))
    (when (null added-flag) (setq *sprites* (cons sprite *sprites*)))
    (restore-sprites (vec2-x pos) (vec2-y pos) (image-width image) (image-height image))
    sprite))

(defun set-sprite-pos (sprite pos)
  "Установить позицию переданного спрайта"
  (let ((oldx (vec2-x (sprite-pos sprite)))
	(oldy (vec2-y (sprite-pos sprite))))
    (sprite-set-pos sprite pos)
    (restore-sprites oldx oldy (image-width (sprite-image sprite)) (image-height (sprite-image sprite))))
  (restore-sprites
   (vec2-x pos)
   (vec2-y pos)
   (image-width (sprite-image sprite))
   (image-height (sprite-image sprite))))

(defun delete-sprite (sprite)
  "Удалить переданный спрайт"
  (setq *sprites* (filter #'(lambda (spr) (not (eq spr sprite))) *sprites*))
  (let ((pos (sprite-pos sprite))
	(width (image-width (sprite-image sprite)))
	(height (image-height (sprite-image sprite))))
    (restore-sprites (vec2-x pos) (vec2-y pos) width height)))

;--------------------------------------сверху API-----------------------------
(defun draw-sprite (sprite)
  "Отрисовать переданный спрайт"
  (gsave)
  (translate (sprite-pos sprite))
  (draw-image (sprite-image sprite))
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
	      (translate (cons (* rowsize (* -1 *tile-w*)) *tile-h*))))
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

(defun restore-sprites (x y width height)
  "Восстанавливает спрайты в заданном регионе"
  (draw-part-background x y width height)
  (let* ((x2 (+ x width *tile-w*))
	 (y2 (+ y height *tile-h*))
	 (x (- x *tile-w*))
	 (y (- y *tile-h*)))
    (app #'(lambda (spr)
	     (let* ((sprx (car (sprite-pos spr)))
		    (spry (cdr (sprite-pos spr)))
		    (sprx2 (+ sprx (image-width (sprite-image spr))))
		    (spry2 (+ spry (image-height (sprite-image spr)))))
	       (when (or
		      (and (and (> sprx x) (< sprx x2)) (and (> spry y) (< spry y2)))
		      (and (and (> sprx2 x) (< sprx2 x2)) (and (> spry2 y) (< spry2 y2))))
		   (draw-sprite spr))))
	 *sprites*)))
  
