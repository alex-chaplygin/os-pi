;Математические функции

(defvar *seed* 2147483647) ; зерно генератора псевдослучайных чисел
(defvar *max-int* 0x7fffffff) ; маска максимального неотрицательного числа
(defconst +rand-mult+ 31415) ; множитель для генерации псевдослучайных чисел
(defconst +rand-add+ 63617) ; слагаемое для генерации псевдослучайных чисел

(defun set-seed (new-seed)
  "Установить зерно генератора псевдослучайных чисел"
  (setq *seed* new-seed))

(defun rand ()
  "Получить следующее значение зерна генератора псевдослучайных чисел"
  (setq *seed* (+ (& (* *seed* +rand-mult+) *max-int*) +rand-add+)))

(defun randint (min max)
  "Получить случайное число от min до max"
  (+ (% (rand) (- (++ max) min)) min))

(defun abs (x)
  "Абсолютное значение числа"
  (if (< x 0) (- 0 x) x))

(defun expt (x y)
  "Возведение числа в целую степень"
  (if (equal y 0) 1
    (let ((s x))
      (for xx 1 y
        (setq s (* s x)))
      s)))
