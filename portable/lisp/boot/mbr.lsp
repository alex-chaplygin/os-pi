; Смещение таблицы разделов в MBR
(defvar +partition-table-ofs+ 0x1BE)

(defvar +FAT32-type+ 0x0C)

;;структура раздела FAT
(defvar partition-entry '((attributes . 1) ; атрибуты диска
			  (CHS-start . 3);адрес через номера цилиндра, головки и сектора
			  (type . 1); тип раздела
			  (CHS-last . 3)
			  (start-sector . 4)
			  (num-sectors . 4)))


(defun arr-get-num (arr ofs size)
  "Прочесть из массива arr по смещению ofs size байт"
  (if (equal size 0) 0
    (let ((it (- size 1)))
      (+ (<< (aref arr (+ ofs it)) (* it 8)) (arr-get-num arr ofs it)))))

(defun struct-fields (struct arr ofs)
   (car (foldl '(lambda (acc elem)
	    (let ((list (car acc))
		  (ofs (cdr acc))
		  (field (car elem))
		  (size (cdr elem)))
	      (cons (append list (list `(,field (arr-get-num ,arr ,ofs ,size))))
		    (+ ofs size))))
	  (cons nil ofs) struct)))
					; (with-struct struct arr ofs body)
					; (let ((attr (arr-get-num arr ofs num))
					;       (type .. ))
					;         body)
(defun with-struct (struct arr ofs &rest body)
  "Выполнить вычисление body и установить значения переменных структуры struct из массива arr по смещению ofs"
  `(let ,(struct-fields struct arr ofs) ,@body))

(setq struct '((a . 1)(b . 2)(c . 1)))
(setq arr #(0 1 1 3))
(foldl '(lambda (acc elem)
	    (let ((list (car acc))
		  (ofs (cdr acc))
		  (field (car elem))
		  (size (cdr elem)))
	      elem)) (car struct) struct)
(struct-fields struct arr 0)
;(with-struct struct arr 0
;	     c)
(defvar sec0(ata-read-sectors 0 0 1))
(defun show-mbr-info (xxx)
  (eval (with-struct partition-entry sec0 +partition-table-ofs+
  '(- +FAT32-type+ (aref sec0 (+ +partition-table-ofs+ 4)))
  '(list (cons 'CHS-start CHS-start) (cons 'type  type) (cons 'CHS-last CHS-last) (cons 'start-sector  start-sector) (cons 'num-sectors  num-sectors)))))
(show-mbr-info 0)
;Чтение disk.qcow2; 
(defun load-partition (disk-num part-num)
(let ((sec (ata-read-sectors disk-num 0 1)))
  (eval (with-struct partition-entry sec (+ (* 16 part-num) +partition-table-ofs+)
     '(ata-read-sectors disk-num start-sector 1) ))))
(load-partition 0 0)