;;Декодирование минимальной единицы 8x8 jpeg

(defconst +unit-len+ 64)

(defvar *pred* #(0 0 0)) ;Предыдущее значение DC

;; (defun decode-data-unit (id dc ac)
;;   "ac - таблица Хаффмана для AC, dc - таблица Хаффмана для DC"
;;   (let ((unit (make-array +unit-len+)))
;;     (seta unit 0 (decode-dc dc id))
;;     (setq unit (decode-ac ac unit))
;;   unit))
;; ;;(inverse-cos)
;; ;;(zig-zag)

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

(defun decode-dc (huff)
  (&&& num-> (huff-decode huff)
       numbit-> (parse-bits num)
       return (decode-sign numbit num)))

(defun decode-ac (huff)
  "Декодирование AC-коэффициента"
  #'(lambda (stream)
      (let ((arr (make-array +unit-len+))
	    (count 1)
	    (val 1))
	(for i 0 +unit-len+ (seta arr i 0))
	(while (!= val 0)
	       (let ((res (funcall (huff-decode huff) stream)))
		 (setq val (car res))
		 (if (= val 0xf) (setq count (+ count 16))
		     (let* ((size (& val 0xf))
			    (r (get-bits (cdr res) size))
			    (num (car r)))
		       (setq count (+ count (>> val 4)))
		       (seta arr count (decode-sign num size))
		       (incf count)
		       (when (= count +unit-len+) (setq val 0))
		       (setq stream (cdr r))))))
	(cons arr stream))))

(defun decode-block (huff-dc huff-ac quant)
  "Декодирование блока с таблицами Хаффмана для dc и ac и таблицей квантования"
  (&&& dc-> (decode-dc huff-dc)
       coefs-> (decode-ac huff-ac)
       return (progn (seta coefs 0 dc) (level-shift (idct (unzip (dequant coefs quant)))))))

(defun decode-mcu (hufs)
  nil)
