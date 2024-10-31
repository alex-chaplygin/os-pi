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
  "Переобпредлённая функция о"
  (setq *hlist* (append *hlist* (list (list x1 x2 y)))))
  

(test-fill-triangle '(5 . 1) '(1 . 3) '(8 . 8) '((4 5 2) (3 5 3) (4 5 4) (5 6 5)))
(test-next-x 1 1 4 4 '(2 3 4))
(test-next-x 3 2 5 6 '(3 4 4 5))
(test-next-x 6 6 2 8 '(4 2))
(test-next-x 3 3 3 6 '(3 3 3))

