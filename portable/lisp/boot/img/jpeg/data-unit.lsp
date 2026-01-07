;;Декодирование
(defconst +unit-len+ 64)
(defvar *pred*) ;Предыдущее значение DC

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
  "Декодирование DC-коэффициента"
  (&&& num-> (huff-decode huff)
       numbit-> (parse-bits num)
       return (decode-sign numbit num)))

(defun decode-ac (huff)
  "Декодирование AC-коэффициента"
  #'(lambda (stream)
      (let ((arr (make-array +unit-len+))
	    (count 1)
	    (flag 1))
	(for i 0 +unit-len+ (seta arr i 0))
	(while flag
	       (let* ((res (funcall (huff-decode huff) stream))
		      (val (car res))
		      (size (& val 0xf))
		      (skip (>> val 4)))
		 (setq stream (cdr res))
		 (if (= size 0) (if (= skip 15) (setq count (+ count 16)) (setq flag nil))
		     (let* ((r (get-bits stream size))
			    (num (car r)))
		       (setq stream (cdr r))
		       (setq count (+ count skip))
		       (seta arr count (decode-sign num size))
		       (if (= count (-- +unit-len+)) (setq flag nil) (incf count))))))
	(cons arr stream))))

(defun decode-block (id huffs quant)
  "Декодирование блока с каналом id с таблицами Хаффмана для dc и ac и таблицей квантования"
  (&&& dc-> (decode-dc (car huffs))
       coefs-> (decode-ac (cdr huffs))
       return (progn (setq dc (+ (aref *pred* id) dc))
		     (seta *pred* id dc)
		     (seta coefs 0 dc)
		     (level-shift (idct (unzip (dequant coefs quant)))))))

(defun decode-mcu (hufs quants hvs)
  "Декодировать MCU с таблицами Хаффмана, квантования, коэффициентами прореживания"
  (let ((hvy (aref hvs 0))
	(hvcb (aref hvs 1))
	(hvcr (aref hvs 2)))
    (labels ((hvsize (hv) (* (car hv) (cdr hv))))
      (&&& (parse-many-n (hvsize hvy) (decode-block 0 (aref hufs 0) (aref quants 0)))
	   (parse-many-n (hvsize hvcb) (decode-block 1 (aref hufs 1) (aref quants 1)))
	   (parse-many-n (hvsize hvcr) (decode-block 2 (aref hufs 2) (aref quants 2)))))))
