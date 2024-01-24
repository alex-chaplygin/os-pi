; Вид матрицы:
; (a  b  0)
; (c  d  0)
; (tx ty 1)

; Сокращённо: (a b c d tx ty)

; x' = a*x + c*y + tx
; y' = b*x + d*y + ty

(defun mat-make (a b c d tx ty)
    "Создание матрицы трансформации"
    `#(,a ,b ,c ,d ,tx ,ty))

(defun mat-id ()
    "Создание единичной матрицы"
    (mat-make 1 0 0 1 0 0))

(defun mat-mul (m1 m2)
    "Произведение матриц"
    (mat-make (+ (* (aref m1 0) (aref m2 0))
                 (* (aref m1 1) (aref m2 2)))
              (+ (* (aref m1 0) (aref m2 1))
                 (* (aref m1 1) (aref m2 3)))
              (+ (* (aref m1 2) (aref m2 0))
                 (* (aref m1 3) (aref m2 2)))
              (+ (* (aref m1 2) (aref m2 1))
                 (* (aref m1 3) (aref m2 3)))
              (+ (* (aref m1 4) (aref m2 0))
                 (* (aref m1 5) (aref m2 2))
                 (* 1           (aref m2 4)))
              (+ (* (aref m1 4) (aref m2 1))
                 (* (aref m1 5) (aref m2 3))
                 (* 1           (aref m2 5)))))

(defun mat-trans (m1 x y)
    "Применение смещения"
    (mat-mul m1 (mat-make 1 0 0 1 x y)))

(defun mat-scale (m1 x y)
    "Применение масштабирования"
    (mat-mul m1 (mat-make x 0 0 y 0 0)))

(defun mat-mul-vec (m v)
    "Произведение матрицы на вектор"
    (cons (+ (* (aref m 0) (car v))
              (* (aref m 2) (cdr v))
              (aref m 4))
          (+ (* (aref m 1) (car v))
              (* (aref m 3) (cdr v))
              (aref m 5))))