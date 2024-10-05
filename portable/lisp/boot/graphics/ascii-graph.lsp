;Вывод растровой графики в ASCII
(defvar *screen*)
(defvar *tile-w* 1)
(defvar *tile-h* 1)
(defvar *tile-hash*)

(defun set-screen (screen)
  "Задать матрицу экрана"
  "set-screen #(#(WALL PLAT WALL PLAT)
		#(nil WALL nil))"
  (setq *screen* screen))

(defun draw-screen ()
  "Отрисовать экран"
  (draw-background))

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
  "Установить размеры спрайта по умолчанию")

(defun new-sprite (tile x y)
  "Создать новый спрайт с заданными координатами"
  )

(defun set-sprite-pos (sprite x y)
  "Установить позицию переданного спрайта")

(defun set-sprite-tile (sprite tile)
  "Установить тайл для переданного спрайта")

(defun set-sprite-size (sprite w h)
  "Установить размер переданного спрайта")

;--------------------------------------сверху API-----------------------------
(defun draw-tile (tile x y)
  (for yy y (+ *tile-h* y)
       (set-cursor x yy)
       (for xx x (+ *tile-w* x) (putchar (get-hash *tile-hash* tile)))))

(defun draw-background ()
  "Отрисовать сетку тайлов"
  (for r 0 (array-size *screen*)
       (let ((row (aref *screen* r)))
	 (for c 0 (array-size row)
	      (draw-tile (aref row c) (* *tile-w* c) (* *tile-h* r))))))
