;;Декодирование минимальной единицы 8x8 jpeg

(defconst +unit-len+ 64)

(defvar *pred*) ;Предыдущее значение DC

(defun decode-data-unit (id ac dc)
  "ac - таблица Хаффмана для AC, dc - таблица Хаффмана для DC"
  (setq *pred* (make-array *num-components*))
  (let ((unit (make-array +unit-len)))
    (seta *pred* id 0)
    (seta unit 0 (decode-dc dc id))
  unit))
;;(decode-ac)
;;(dequant)
;;(inverse-cos)
;;(zig-zag)

(defun decode-sign (num size)
  "Если старший бит 1, то убирает его и возвращает отриацательный остаток"
  "Проверяет число num длиной size"
  (let ((temp num) (count 0))
    (while (< count (-- size))
      (setq temp (>> temp 1))
      (setq count (++ count)))
    (when (= temp 1)
	(progn
	  (setq num (- num (expt 2 count)))
	  (setq num (* num -1))))
    num))

(defun decode-dc (huff id)
  "Декодирование DC-коэффициента"
  (let* ((t (huff-decode huff))
	 (diff (decode-sign (get-bits t) t))
	 (dc (+ diff (aref pred id))))
    (seta pred id dc)
    dc))
