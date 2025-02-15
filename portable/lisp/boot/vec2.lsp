(defconst to-rad (/ 3.14159265 180)) ; Константа для упрощенного перевода координат.

(defun make-vec2 (x y)
  "Создать двумерный вектор"
  (cons x y))

;Конструктор двумерного вектора
;Аргументы функции: Угол в градусах, длина вектора
;Функция возвращает двумерный вектор

(defun make-vec2-polar (angle len)
  (let ((x (* len (COS (* angle to-rad))))
	(y (* len (SIN (* angle to-rad)))))
    (make-vec2 x y)))

(defun vec2-x (v)
  "Получить x координату вектора"
  (car v))

(defun vec2-y (v)
  "Получить y координату вектора"
  (cdr v))

(defun vec2-add (v1 v2)
  "Сложить два вектора"
  (make-vec2 (+ (vec2-x v1) (vec2-x v2)) (+ (vec2-y v1) (vec2-y v2))))
  
(defun vec2-sub (v1 v2)
  "Вычесть два вектора"
  (make-vec2 (- (vec2-x v1) (vec2-x v2)) (- (vec2-y v1) (vec2-y v2))))

(defun vec2-scale (v s)
  "Умножить вектор на скаляр"
  (make-vec2 (* (vec2-x v) s) (* (vec2-y v) s)))
  
(defun vec2-length (v)
  "Вычислить длину вектора"
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (sqrt (+ (* x x) (* y y)))))
