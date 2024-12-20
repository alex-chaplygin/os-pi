(defvar *hlist*) ;список горизонталей для теста fill-triangle

(defun test-next-x (x1 y1 x2 y2 res-list)
  "Тестирование поиска пересечения горизонталью ребра"
  (let ((e (make-Edge x1 y1 y2 (- x2 x1) (- y2 y1) 0))
	(list nil))
    (for i y1 y2
	 (setq list (append list (list (next-x e)))))
    (print (assert list res-list))))
  
(defun test-fill-triangle (p1 p2 p3 hlines)
  "Тестирование заполнения треугольника p1p2p3, hlines -  список горизонтальных линей"
  (setq *hlist* nil)
  (fill-triangle p1 p2 p3 1)
  (print (assert *hlist* hlines)))

(defun draw-hline (x1 x2 y colour)
  "Переобпредлённая функция рисования горизонтальной линии"
  (setq *hlist* (append *hlist* (list (list x1 x2 y)))))

(defun test-bezier-curve ()
  (print "Тестирование рисования кривой Безье")
  (setq *points* nil)
  (draw-bezier-curve '(50 . 50) '(50 .120) '(100 . 0) '(100 . 80) 5 4)
;  (print *points*))
  )

(defun set-pixel (x y colour)
  "Заглушка рисования точки"
  (print `(,x ,y))
  (setq *points* (append *points* (list x y))))

(defun fill-triangle-tests ()
  (print "Произвольный треугольник")
  (test-fill-triangle '(5 . 1) '(1 . 3) '(8 . 8) '((4 4 2) (2 5 3) (3 5 4) (5 6 5) (6 6 6)))
  (test-next-x 5 1 1 3 '(3 1))
  (test-next-x 5 1 8 8 '(5 6 6 7 7 8 8))
  (test-next-x 1 3 8 8 '(2 4 5 7 8))

  (print "Треугольник с горизонтальной нижней гранью")
  (test-fill-triangle '(6 . 1) '(1 . 6) '(10 . 6) '((6 6 2) (5 7 3) (4 7 4) (3 8 5)))
  (test-next-x 6 1 1 6 '(5 4 3 2 1))
  (test-next-x 6 1 10 6 '(7 8 8 9 10))
  (test-next-x 1 6 10 6 ())

  (print "Треугольник с горизонтальной верхней гранью")
  (test-fill-triangle '(1 . 1) '(11 . 1) '(8 . 8) '((3 10 2) (4 9 3) (5 9 4) (6 8 5) (7 8 6)))
  (test-next-x 1 1 11 1 ())
  (test-next-x 1 1 8 8 '(2 3 4 5 6 7 8))
  (test-next-x 11 1 8 8 '(11 10 10 9 9 8 8))

  (print "Несортированные треугольники")
  (test-fill-triangle '(8 . 8) '(1 . 3) '(5 . 1) '((4 4 2) (2 5 3) (3 5 4) (5 6 5) (6 6 6)))
  (test-fill-triangle '(6 . 1) '(10 . 6) '(1 . 6) '((6 6 2) (5 7 3) (4 7 4) (3 8 5)))
  (test-fill-triangle '(11 . 1) '(1 . 1) '(8 . 8) '((3 10 2) (4 9 3) (5 9 4) (6 8 5) (7 8 6)))
  (test-fill-triangle '(10 . 6) '(1 . 6) '(6 . 1) '((6 6 2) (5 7 3) (4 7 4) (3 8 5)))
  (test-fill-triangle '(11 . 1) '(8 . 8) '(1 . 1) '((3 10 2) (4 9 3) (5 9 4) (6 8 5) (7 8 6)))

  (test-next-x 1 1 4 4 '(2 3 4))
  (test-next-x 3 2 5 6 '(4 4 5 5))
  (test-next-x 6 6 2 8 '(4 2))
  (test-next-x 3 3 3 6 '(3 3 3))
  (test-next-x 1 1 2 10 '(1 1 1 1 2 2 2 2 2))
  (test-next-x 1 1 4 8 '(1 2 2 3 3 4 4)))

(defun test-mid-point (x1 y1 x2 y2 colour points)
  "Тестирование алгоритма средней точки"
  (setq *points* nil)
  (draw-line x1 y1 x2 y2 colour)
  (print (assert *points* points)))

;(fill-triangle-tests)
;(test-bezier-curve)
(test-mid-point 2 2 4 6 1 '(2 2 2 3 3 4 3 5 4 6))
(test-mid-point 8 8 3 4 1 '(8 8 7 7 6 6 5 6 4 5 3 4))
(test-mid-point 8 2 3 16 1 '(8 2 8 3 7 4 7 5 7 6 6 7 6 8 6 9 5 10 5 11 4 12 4 13 4 14 3 15 3 16))
(test-mid-point 3 20 15 4 1 '(3 20 4 19 4 18 5 17 6 16 7 15 7 14 8 13 9 12 10 11 10 10 11 9 12 8 13 7 13 6 14 5 15 4))

(test-mid-point 5 5 15 5 1 '(5 5 6 5 7 5 8 5 9 5 10 5 11 5 12 5 13 5 14 5 15 5))
(test-mid-point 5 15 5 5 1 '(5 15 5 14 5 13 5 12 5 11 5 10 5 9 5 8 5 7 5 6 5 5))
(test-mid-point 8 8 8 20 1 '(8 8 8 9 8 10 8 11 8 12 8 13 8 14 8 15 8 16 8 17 8 18 8 19 8 20))
(test-mid-point 8 20 8 8 1 '(8 20 8 19 8 18 8 17 8 16 8 15 8 14 8 13 8 12 8 11 8 10 8 9 8 8))

(test-mid-point -5 -4 7 6 1 '(-5 -4 -4 -3 -3 -2 -2 -2 -1 -1 0 0 1 1 2 2 3 3 4 3 5 4 6 5 7 6))
;(test-mid-point -50 -48 -44 -42 1 '(-50 -48 -49 -47 -48 -46 -47 -45 -46 -44 -45 -43 -44 -42))

(test-mid-point 37 17 176 200 1 '())
