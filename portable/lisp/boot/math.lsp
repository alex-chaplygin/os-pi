;Математические функции
(defconst PI 3.14159265359)
(defvar *seed* 1) ; зерно генератора псевдослучайных чисел
(defvar *max-int* 0xffff) ; маска максимального неотрицательного числа
(defconst +rand-mult+ 1103515245); новый множитель для генерации псевдослучайных чисел
(defconst +rand-add+ 12345) ; слагаемое для генерации псевдослучайных чисел

(defun set-seed (new-seed)
  "Установить зерно генератора псевдослучайных чисел"
  (setq *seed* new-seed))

(defun rand ()
  "Получить следующее значение зерна генератора псевдослучайных чисел"
  (setq *seed* (% (abs(+ (* *seed* +rand-mult+) +rand-add+)) *max-int*)))

(defun randint (min max)
  "Получить случайное число от min до max"
  (+ (% (rand) (- (++ max) min)) min))

(defun expt (x y)
  "Возведение числа в целую степень"
  (if (equal y 0) 1
    (let ((s x))
      (for xx 1 y
        (setq s (* s x)))
      s)))

