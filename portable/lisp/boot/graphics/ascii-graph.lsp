;Вывод растровой графики в ASCII
(defvar *screen*)
(defvar *tile-w* 1)
(defvar *tile-h* 1)
(defvar *tile-hash*)
(defvar *default-sprite-w*)
(defvar *default-sprite-h*)
(defvar *sprite-list* nil)
(defclass Sprite () (tile x y width height))

(defun set-screen (screen)
  "Задать матрицу экрана"
  "set-screen #(#(WALL PLAT WALL PLAT)
		#(nil WALL nil))"
  (setq *screen* screen))

(defun draw-screen ()
  "Отрисовать экран"
  (draw-background)
  (draw-sprites))

(defun set-tile-size (w h)
  "Установить размеры тайла"
  "set-tile-size 20 10"
  (setq *tile-w* w)
  (setq *tile-h* h))

(defun set-tiles (tiles)
  "Задать список тайлов"
  "set-tiles '((WALL . \"#\") (PLAT . \"-\"))"
  (setq *tile-hash* (make-hash))
  (app '(lambda (tile) (set-hash *tile-hash* (car tile) (cdr tile))) tiles))

(defun set-default-sprite-size (w h)
  "Установить размеры спрайта по умолчанию"
  (setq *default-sprite-w* w)
  (setq *default-sprite-h* h))

(defun new-sprite (tile x y z)
  "Создать новый спрайт с заданным тайлом и координатами"
  (let ((sprite (make-Sprite tile x y *default-sprite-w* *default-sprite-h*)))
    (setq *sprite-list* (append *sprite-list* (list sprite)))
    sprite))

(defun set-sprite-pos (sprite x y z)
  "Установить позицию переданного спрайта"
  (setf (slot sprite 'x) x)
  (setf (slot sprite 'y) y))

(defun set-sprite-tile (sprite tile)
  "Установить тайл для переданного спрайта"
  (setf (slot sprite 'tile) tile))

(defun set-sprite-size (sprite w h)
  "Установить размер переданного спрайта"
  (setf (slot sprite 'w) w)
  (setf (slot sprite 'h) h))

;--------------------------------------сверху API-----------------------------
(defun draw-tile (tile x y)
  "Отрисовать переданный тайл с заданными координатами"
  (for yy y (+ *tile-h* y)
       (set-cursor x yy)
       (for xx x (+ *tile-w* x) (putchar (get-hash *tile-hash* tile)))))

(defun draw-sprites ()
  "Отрисовать спрайты из списка спрайтов"
  (app '(lambda (sprite) (draw-sprite sprite)) *sprite-list*))

(defun draw-sprite (sprite)
  "Отрисовать переданный спрайт"
  (for y (slot sprite 'y) (+ (slot sprite 'height) (slot sprite 'y))
       (set-cursor (slot sprite 'x) y)
       (for x (slot sprite 'x) (+ (slot sprite 'width) (slot sprite 'x))
	    (putchar (get-hash *tile-hash* (slot sprite 'tile))))))
  
(defun draw-background ()
  "Отрисовать сетку тайлов"
  (for r 0 (array-size *screen*)
       (let ((row (aref *screen* r)))
	 (for c 0 (array-size row)
	      (draw-tile (aref row c) (* *tile-w* c) (* *tile-h* r))))))
