; Вид матрицы:
; (a  b  0)
; (c  d  0)
; (tx ty 1)

; Сокращённо: (a b c d tx ty)

; x' = a*x + c*y + tx
; y' = b*x + d*y + ty

(defun make-ctm (a b c d tx ty)
    "Создание матрицы трансформации"
    (setq m (make-array 6))
    (seta m 0 a)
    (seta m 1 b)
    (seta m 2 c)
    (seta m 3 d)
    (seta m 4 tx)
    (seta m 5 ty)
    m)

(defun mul-mat (m1 m2)
    "Произведение матриц"
    (setq m1a m1)
    (setq m2a m2)
    (make-ctm (+ (* (aref m1a 0) (aref m2a 0))
                 (* (aref m1a 1) (aref m2a 2)))
              (+ (* (aref m1a 0) (aref m2a 1))
                 (* (aref m1a 1) (aref m2a 3)))
              (+ (* (aref m1a 2) (aref m2a 0))
                 (* (aref m1a 3) (aref m2a 2)))
              (+ (* (aref m1a 2) (aref m2a 1))
                 (* (aref m1a 3) (aref m2a 3)))
              (+ (* (aref m1a 4) (aref m2a 0))
                 (* (aref m1a 5) (aref m2a 2))
                 (* 1            (aref m2a 4)))
              (+ (* (aref m1a 4) (aref m2a 1))
                 (* (aref m1a 5) (aref m2a 3))
                 (* 1            (aref m2a 5)))))

(defun apply-move (m1 x y)
    "Применение смещения"
    (mul-mat m1 (make-ctm 1 0 0 1 x y)))

(defun apply-scale (m1 x y)
    "Применение масштабирования"
    (mul-mat m1 (make-ctm x 0 0 y 0 0)))