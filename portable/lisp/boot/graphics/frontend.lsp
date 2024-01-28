(defvar *path-begin* (cons 0 0))     ; Последняя точка пути
(defvar *path-cur* (cons 0 0))       ; Текущая точка пути
(defvar *cur-state* (make-hash))     ; Текущее состояние
(set-hash *cur-state* 'ctm (mat-id)) ; Матрица трансформации
(set-hash *cur-state* 'color 0)      ; Цвет
(defvar *sav-states* (list))         ; Список сохраненных состояний

(defmacro save-state ()
  "Сохранение состояния"
  `(setq *sav-states* (cons *cur-state* *sav-states*)))

(defmacro restore-state ()
  "Восстановление состояния"
  `(setq *cur-state* (car *sav-states*))
  `(setq *sav-states* (cdr *sav-states*)))

(defmacro matrix (a b c d tx ty)
  "Произведение матрицы трансформации"
  `(mat-mul (get-hash *cur-state* 'ctm) (mat-make ,a ,b ,c ,d ,tx ,ty)))

(defmacro begin-path (x y)
  "Начать новый контур"
  `(setq *path-begin* (cons ,x ,y))
  `(setq *path-cur* (cons ,x ,y)))

(defmacro line-to (x y)
  "Рисование линии до точки"
  `(let* ((ctm (get-hash *cur-state* 'ctm))
          (xy1 (mat-mul-vec ctm *path-cur*))
          (xy2 (mat-mul-vec ctm (cons ,x ,y))))
    (draw-line (car xy1) (cdr xy1) (car xy2) (cdr xy2) (get-hash *cur-state* 'color))
    (setq *path-cur* (cons ,x ,y))))

(defmacro cubic-bezier (x1 y1 x2 y2 x3 y3 x4 y4)
  "Рисование кубической кривой Безье"
  `(draw-bezier-curve ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 (get-hash *cur-state* 'color)))

(defmacro close-path ()
  "Завершение текущего пути"
  `(let* ((ctm (get-hash *cur-state* 'ctm)))
    (line-to (car *path-begin*) (cdr *path-begin*))))

(defmacro stroke-path ()
  "Рисование текущего пути"
  )

(defmacro rectangle (x y w h)
  "Рисование прямоугольника"
  `(let* ((ctm (get-hash *cur-state* 'ctm)))
    (begin-path ,x ,y)
    (line-to (+ ,x ,w) ,y)
    (line-to (+ ,x ,w) (+ ,y ,h))
    (line-to ,x (+ ,y ,h))
    (close-path)))

(defmacro set-colour (color)
  "Установка цвета"
  `(set-hash *cur-state* 'color ,color))

(defmacro translate (x y)
  "Смещение"
  `(set-hash *cur-state* 'ctm (mat-trans (get-hash *cur-state* 'ctm) ,x ,y)))

(defmacro scale (x y)
  "Масштабирование"
  `(set-hash *cur-state* 'ctm (mat-scale (get-hash *cur-state* 'ctm) ,x ,y)))

(defun frontend-test ()
  (bgr-set-res +screen-width+ +screen-height+ +screen-depth+)
  (set-colour 1)
					; (translate 100 100)
					; (scale 2 1)
  (begin-path 100 50)
  (line-to 200 100)
  (line-to 50 100)
  (close-path)
					; (cubic-bezier 10 10 200 10 200 150 50 150)
  (set-colour 2)
  (rectangle 10 10 30 30)
  (translate 50 10)
  (set-colour 3)
  (begin-path 100 50)
  (line-to 200 100)
  (line-to 50 100)
  (close-path)
  (scale 1 2)
  (set-colour 4)
  (rectangle 10 10 30 30)
  (graph-send-buffer *gr-buf*))
