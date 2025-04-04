(defvar *new-path* nil)              ; Список строк
(defvar *path-begin* (cons 0 0))     ; Последняя точка пути
(defvar *cur-point* nil)             ; Текущая точка
(defvar *cur-state* (make-hash))     ; Текущее состояние
(set-hash *cur-state* 'ctm (mat-id)) ; Матрица трансформации
(set-hash *cur-state* 'color 0)      ; Цвет
(defvar *sav-states*)         ; Список сохраненных состояний

(defmacro gsave ()
  "Сохранение состояния"
  `(setq *sav-states* (cons (clone *cur-state*) *sav-states*)))

(defun grestore ()
  "Восстановление состояния"
  (setq *cur-state* (car *sav-states*))
  (setq *sav-states* (cdr *sav-states*)))

(defmacro matrix (a b c d tx ty)
  "Произведение текущей матрицы трансформации на матрицу a b c d tx ty"
  `(mat-mul (get-hash *cur-state* 'ctm) (mat-make ,a ,b ,c ,d ,tx ,ty)))

(defmacro new-path ()
  "Начать новый контур"
  `(setq *new-path* nil))

(defmacro move-to (x y)
  "Переместить текущую точку в контуре в точку p"
  `(setq *cur-point* (cons ,x ,y))
  `(when (null *new-path*) (setq *path-begin* *cur-point*)))

(defmacro line-to (x y)
  "Добавить прямую линию от текущей точки до точки p"
   `(let* ((ctm (get-hash *cur-state* 'ctm))
          (p1 (mat-mul-vec ctm *cur-point*))
          (p2 (mat-mul-vec ctm (cons ,x ,y))))
      (setq *new-path* (append *new-path* (list (list 'LINE (cons p1 p2))))))
  `(move-to ,x ,y))

(defmacro stroke-path ()
  "Обвести текущий контур и добавить точки в область отрисовки"
  `(dolist (line *new-path*)
     (case (car line)
       ('LINE (let ((xy1 (caadr line))
		    (xy2 (cdadr line)))
               (progn
                 (screen-add-point xy1)
                 (screen-add-point xy2)
		 (draw-line (car xy1) (cdr xy1) (car xy2) (cdr xy2) (get-hash *cur-state* 'color))))))))

(defmacro cubic-bezier (x1 y1 x2 y2 x3 y3 x4 y4)
  "Рисование кубической кривой Безье"
  `(draw-bezier-curve ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 (get-hash *cur-state* 'color)))

(defmacro close-path ()
  "Закрыть текущий контур"
  `(line-to ,(car *path-begin*) ,(cdr *path-begin*)))

(defmacro rectangle (x y w h)
  "Рисование прямоугольника"
  `(new-path)
  `(move-to ,x ,y)
  `(line-to (+ ,x ,w) ,y)
  `(line-to (+ ,x ,w) (+ ,y ,h))
  `(line-to ,x (+ ,y ,h))
  `(line-to ,x ,y)
  `(stroke-path))

(defmacro set-colour (color)
  "Установка цвета"
  `(set-hash *cur-state* 'color ,color))

(defun translate (p)
  "Смещение"
  (set-hash *cur-state* 'ctm (mat-trans (get-hash *cur-state* 'ctm) (car p) (cdr p))))

(defmacro scale (x y)
  "Масштабирование"
  `(set-hash *cur-state* 'ctm (mat-scale (get-hash *cur-state* 'ctm) ,x ,y)))

(defmacro rotate (a)
  "Поворот"
  `(set-hash *cur-state* 'ctm (mat-rotate (get-hash *cur-state* 'ctm) ,a)))
