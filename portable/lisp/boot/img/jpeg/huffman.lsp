(defun arr-get-sum (arr size)
  "Вычисление суммы элементов массива arr длиной size"
  (let ((sum 0))
    (for i 0 size
	 (setq sum (+ sum (aref arr i))))
    sum))
	 
(defun make-len-arr (len)
  "Создать массив длин из массива по длинам len"
  (let ((ans (make-array (++ (arr-get-sum len 16))))
	(count 0))
    (for i 0 16
	 (let ((temp (aref len i)))
	   (when (not (= 0 temp))
	     (for j 0 temp
		  (seta ans count (++ i))
		  (setq count (++ count))))))
    (seta ans count 0)
    ans))
	   
(defun get-huff-table (len V)
  "Создание и заполнение таблицы Хаффмана по массиву длин кодов len"
  (let ((huff (make-huff))
	(c 0)
	(s 1)
	(k 0)
	(not-end t)
	(i 0))
    ;(print `(len ,len))
    ;(print `(v ,v))
    (while not-end
      (while (= s (aref len k))
	(setq huff (huff-add huff c s (aref v i)))
	(incf i)
	(setq c (++ c))
	(setq k (++ k)))
      (if (not (= 0 (aref len k)))
	  (progn
	    (setq c (<< c 1))
	    (setq s (++ s))
	    (while (not (= s (aref len k)))
	      (setq c (<< c 1))
	      (setq s (++ s))))
	  (setq not-end nil)))
    huff))

