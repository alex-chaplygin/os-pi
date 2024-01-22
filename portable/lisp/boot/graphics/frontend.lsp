(defmacro save-state ()
  "Сохранение состояния"
  `((setq *state* (make-hash))
    (set-hash *state* 'ctm *ctm*)
    (set-hash *state* 'color *color*)))

(defmacro restore-state ()
  "Восстановление состояния"
  `((setf *ctm* (get-hash *state* 'ctm))
    (setf *color* (get-hash *state* 'color))))

(defmacro matrix (a b c d tx ty)
  "Произведение матрицы трансформации"
  `(mat-mul *ctm* (mat-make ,a ,b ,c ,d ,tx ,ty)))

(defmacro move-to (x y)
  "Смещение курсора в точку"
  `(translate (- ,x (aref *ctm* 4)) (- ,y (aref *ctm* 5))))

(defmacro line-to (x y)
  "Рисование линии до точки"
  `((draw-line (aref *ctm* 4) (aref *ctm* 5) (* ,x (aref *ctm* 0)) (*,y (aref *ctm* 3)))
    (translate (- ,x (aref *ctm* 4)) (- ,y (aref *ctm* 5)))))

(defmacro cubic-bezier (x1 y1 x2 y2 x3 y3 x4 y4)
  "Рисование кубической кривой Безье"
  `(draw-bezier-curve ,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4 *color*))

(defmacro close-path ()
  "Завершение текущего пути"
  )

(defmacro stroke-path ()
  "Рисование текущего пути"
  )

(defmacro rectangle (x y w h)
  "Рисование прямоугольника"
  `(draw-rect ,x ,y ,w ,h *color*))

(defmacro set-color (color)
  "Установка цвета"
  `(setf *color* ,color))

(defmacro translate (x y)
  "Смещение"
  `(mat-trans *ctm* ,x ,y))

(defmacro scale (x y)
  "Масштабирование"
  `(mat-scale *ctm* ,x ,y))