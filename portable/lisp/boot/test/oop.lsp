(defun oop-test ()
  (defclass point () (x y))
  (defclass angle () (degrees))
  (defclass line point (x2 y2))

  ;; Теститирует геттеры
  (setq p2 (make-point 10 20)) ; x = 10 y = 20
  (print (assert (point-x p2) 10))
  (print (assert (point-y p2) 20))

  ;; Тестирует гетты у класса с родителем
  (setq l1 (make-line 10 20 20 30))
  (print (assert (line-x l1) 10))
  (print (assert (line-y2 l1) 30))
  
  ;; Тестирует сеттеры
  (point-set-x p2 20)
  (print (assert (point-x p2) 20))

  ;; Тестирует сетты у класса с родителем
  (line-set-x l1 15)
  (line-set-y2 l1 35)
  (print (assert (line-x l1) 15))
  (print (assert (line-y2 l1) 35))
  
  ;; Теститрует проверку на типы
  (setq a1 (make-angle 90))
  (print (assert (pointp l1) T))
  (print (assert (pointp a1) ()))
  (print (assert (pointp p2) T))
  
  ;(print (assert (get-slots-count 0) 2))
  ;(print (assert (get-slots-count 1) 4))
  ;(print (make-indexing-list '(x y z) 1))
  ;; (point-set-x p2 50) ; x = 50 y = 20
  ;; (setq l1 (make-line 1 1 3 3))

  (defmethod move ((self point) dx dy)
    (point-set-x self (+ (point-x self) dx))
    (point-set-y self (+ (point-y self) dy)))

  (defmethod move ((self line) dx dy)
     (line-set-x self (+ (line-x self) dx))
     (line-set-y self (+ (line-y self) dy))
     (line-set-x2 self (+ (line-x2 self) dx))
     (line-set-y2 self (+ (line-y2 self) dy)))

  (defmethod mul ((self line) k)
    (line-set-x self (* (line-x self) k))
    (line-set-y self (* (line-y self) k))
    (line-set-x2 self (* (line-x2 self) k))
    (line-set-y2 self (* (line-y2 self) k)))

  ;; Тестирует методы
  (move p2 20 30)
  (print (assert (point-x p2) 40))
  (print (assert (point-y p2) 50))
  ;; Тестирует методы класса с родителем 
  (move l1 20 20)
  (print (assert (line-x l1) 35))
  (print (assert (line-y l1) 40))
  (print (assert (line-x2 l1) 40))
  (print (assert (line-y2 l1) 55))

  ;; Теститрует super
  (super move l1 15 10)
  (print (assert (line-x l1) 50))
  (print (assert (line-y l1) 50))
  (print (assert (line-x2 l1) 40))
  (print (assert (line-y2 l1) 55))
  ;; Тест метода mul
  (mul l1 2)
  (print (assert (line-x l1) 100))
  (print (assert (line-y l1) 100))
  (print (assert (line-x2 l1) 80))
  (print (assert (line-y2 l1) 110))
  
  ;; (print *class-table*)
  ;; (print *class-names*)
  ;; (print *methods-names*)
  ;; (print *methods*)
)
  
(oop-test)
