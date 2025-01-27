;;Декодирование минимальной единицы 8x8 jpeg

(defconst +unit-len+ 64)

(defvar *pred*) ;Предыдущее значение DC

(defun decode-data-unit (id dc ac)
  "ac - таблица Хаффмана для AC, dc - таблица Хаффмана для DC"
  (let ((unit (make-array +unit-len+)))
    (seta unit 0 (decode-dc dc id))
    (setq unit (decode-ac ac unit))
  unit))
;;(inverse-cos)
;;(zig-zag)

(defun decode-sign (num size)
  "Если старший бит 1, то оставляем как есть, иначе преобразуем"
  "Проверяет число num длиной size"
  (let ((temp num) (count 0))
    (while (< count (-- size))
      (setq temp (>> temp 1))
      (setq count (++ count)))
    (when (= temp 0)
      (setq num (+ 1 (- num (expt 2 size)))))
    num))

(defun decode-dc (huff id)
  "Декодирование DC-коэффициента"
  (let* ((num (huff-decode huff))
	 (diff (decode-sign (get-bits num) num))
	 (dc (+ diff (aref *pred* id))))
    (seta *pred* id dc)
    dc))

(defun get-high (num)
  "Получение четырех старших бит у числа num"
  (>> num 4))

(defun get-low (num)
  "Получение четырех старших бит у числа num"
  (& num 0xf))
  
(defun decode-ac (huff unit)
  "Декодирование AC-коэффициента"
  (let ((count 1))
    (while (< count +unit-len+)
      (let* ((num (huff-decode huff))
	     (low (get-low num))
	     (high (get-high num)))
	(if (not (= 0 low))
	    (progn
	      (setq count (+ count high))
	      (seta unit count (decode-sign (get-bits low) low))
	      (setq count (++ count)))
	    (if (= 15 high)
		  (setq count (+ count 16))
		  (setq count +unit-len+)))))
    unit))

(defun dequant (quant unit)
  "Деквантование unit по таблице quant"
  (for i 0 +unit-len+
       (if (equal nil (aref unit i))
	   (seta unit i 0)
	   (seta unit i (* (aref unit i) (aref quant i)))))
  unit)
