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
  (draw-bezier-curve '(0 . 0) '(1 . 2) '(4 . 2) '(5 . 0) 20 1)
  (print *points*))

(defun set-pixel (x y colour)
  "Заглушка рисования точки"
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

(fill-triangle-tests)
(test-bezier-curve)