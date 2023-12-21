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

(eval (with-struct struct arr 0
	'(+ a b c)
	'a))
