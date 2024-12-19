(defvar *m* (mat-make 1 0 0 1 0 0))
(defvar *pi* 3.14)

(defun test-mat-trans (x1 y1 x2 y2)
  "Тестирование применения смещения"
  (let ((m-trans (mat-trans *m* x1 y1)))
    (print (assert (aref m-trans 4) x2))
    (print (assert (aref m-trans 5) y2))))

(defun test-mat-scale (x1 y1 x2 y2)
  "Тестирование применения масштабирования"
  (let ((m-scale (mat-scale *m* x1 y1)))
    (print (assert (aref m-scale 0) x2))
    (print (assert (aref m-scale 3) y2))))

(defun test-mat-rotate (angle a b c d)
  "Тестирование применения поворота матрицы"
  (let ((m-rotated (mat-rotate *m* angle)))
    (print (assert-float (aref m-rotated 0) a))
    (print (assert-float (aref m-rotated 1) b))
    (print (assert-float (aref m-rotated 2) c))
    (print (assert-float (aref m-rotated 3) d))))

(defun mat-trans-tests ()
  (print "Тестирование применения смещения")
  (test-mat-trans 2 3 2 3)
  (test-mat-trans 0 0 0 0)
  (test-mat-trans -1 -2 -1 -2))

(defun mat-scale-tests ()
  (print "Тестирование применения масштабирования")
  (test-mat-scale 2 3 2 3)
  (test-mat-scale 1 1 1 1)
  (test-mat-scale 0.5 0.5 0.5 0.5))

(defun mat-rotate-tests ()
  (print "Тестирование применения поворота")
  (test-mat-rotate 0.0 1.0 0.0 0.0 1.0)
  (test-mat-rotate (/ *pi* 6) 0.866158 0.49977 (- 0 0.49977) 0.866158))

(mat-trans-tests)
(mat-scale-tests)
(mat-rotate-tests)
