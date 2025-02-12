(defun make-vec2 (x y)
  "Создать двумерный вектор"
  (cons x y))

(defun vec2-x (v)
  "Получить x координату вектора"
  (car v))

(defun vec2-y (v)
  "Получить y координату вектора"
  (cdr v))

(defun vec2-add (v1 v2)
  "Сложить два вектора"
  (make-vec2 (+ (vec2-x v1) (vec2-x v2)) (+ (vec2-y v1) (vec2-y v2))))

(defun vec2-scale (v s)
  "Умножить вектор на скаляр"
  (make-vec2 (* (vec2-x v) s) (* (vec2-y v) s)))

(defun vec2-length (v)
  "Вычислить длину вектора"
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (sqrt (+ (* x x) (* y y)))))
