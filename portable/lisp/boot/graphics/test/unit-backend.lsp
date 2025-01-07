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
  (draw-bezier-curve '(50 . 50) '(50 . 120) '(100 . 0) '(100 . 80) 25 1)
  (draw-bezier-curve '(20 . 120) '(30 . 100) '(140 . 100) '(150 . 120) 25 2)
  (draw-bezier-curve '(160 . 100) '(170 . 10) '(300 . 100) '(319 . 100) 25 3)
  (draw-bezier-curve '(30 . 30) '(15 .120) '(100 . 20) '(100 . 100) 25 4)
  (print *points*))
  

(defun set-pixel (x y colour)
  "Заглушка рисования точки"
;  (print `(,x ,y))
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

(fill-triangle-tests)
(test-bezier-curve)
(test-mid-point 2 2 4 6 1 '(2 2 2 3 3 4 3 5 4 6))
(test-mid-point 8 8 3 4 1 '(8 8 7 7 6 6 5 6 4 5 3 4))
(test-mid-point 8 2 3 16 1 '(8 2 8 3 7 4 7 5 7 6 6 7 6 8 6 9 5 10 5 11 4 12 4 13 4 14 3 15 3 16))
(test-mid-point 3 20 15 4 1 '(3 20 4 19 4 18 5 17 6 16 7 15 7 14 8 13 9 12 10 11 10 10 11 9 12 8 13 7 13 6 14 5 15 4))

(test-mid-point 5 5 15 5 1 '(5 5 6 5 7 5 8 5 9 5 10 5 11 5 12 5 13 5 14 5 15 5))
(test-mid-point 5 15 5 5 1 '(5 15 5 14 5 13 5 12 5 11 5 10 5 9 5 8 5 7 5 6 5 5))
(test-mid-point 8 8 8 20 1 '(8 8 8 9 8 10 8 11 8 12 8 13 8 14 8 15 8 16 8 17 8 18 8 19 8 20))
(test-mid-point 8 20 8 8 1 '(8 20 8 19 8 18 8 17 8 16 8 15 8 14 8 13 8 12 8 11 8 10 8 9 8 8))

(test-mid-point -5 -4 7 6 1 '(-5 -4 -4 -3 -3 -2 -2 -2 -1 -1 0 0 1 1 2 2 3 3 4 3 5 4 6 5 7 6))
(test-mid-point -50 -48 -44 -42 1 '(-50 -48 -49 -47 -48 -46 -47 -45 -46 -44 -45 -43 -44 -42))
(test-mid-point 37 17 176 200 1 '(37 17 38 18 39 19 39 20 40 21 41 22 42 23 42 24 43 25 44 26 45 27 45 28 46 29 47 30 48 31 48 32 49 33 50 34 51 35 51 36 52 37 53 38 54 39 54 40 55 41 56 42 57 43 58 44 58 45 59 46 60 47 61 48 61 49 62 50 63 51 64 52 64 53 65 54 66 55 67 56 67 57 68 58 69 59 70 60 70 61 71 62 72 63 73 64 73 65 74 66 75 67 76 68 76 69 77 70 78 71 79 72 80 73 80 74 81 75 82 76 83 77 83 78 84 79 85 80 86 81 86 82 87 83 88 84 89 85 89 86 90 87 91 88 92 89 92 90 93 91 94 92 95 93 95 94 96 95 97 96 98 97 99 98 99 99 100 100 101 101 102 102 102 103 103 104 104 105 105 106 105 107 106 108 107 109 108 110 108 111 109 112 110 113 111 114 111 115 112 116 113 117 114 118 114 119 115 120 116 121 117 122 118 123 118 124 119 125 120 126 121 127 121 128 122 129 123 130 124 131 124 132 125 133 126 134 127 135 127 136 128 137 129 138 130 139 130 140 131 141 132 142 133 143 133 144 134 145 135 146 136 147 137 148 137 149 138 150 139 151 140 152 140 153 141 154 142 155 143 156 143 157 144 158 145 159 146 160 146 161 147 162 148 163 149 164 149 165 150 166 151 167 152 168 152 169 153 170 154 171 155 172 155 173 156 174 157 175 158 176 159 177 159 178 160 179 161 180 162 181 162 182 163 183 164 184 165 185 165 186 166 187 167 188 168 189 168 190 169 191 170 192 171 193 171 194 172 195 173 196 174 197 174 198 175 199 176 200))
