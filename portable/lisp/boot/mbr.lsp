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

(defun struct-names(struct)(map 'car struct));Имена полей структуры
(defun struct-sizes(struct)(map 'cdr struct));Размеры полей структуры
(defun next(n)(let ((x (car iter)))(setq iter (cdr iter))x))
(defun arr-get-nums (arr ofst sizes)(let((ofsts (struct-offsets sizes ofst)))
"Прочесть из массива arr по смещению ofs числа в байтах,указанных списком struct"
 (setq iter sizes)(map '(lambda (ofs) (arr-get-num arr ofs (next 0))) ofsts)))

(defun arr-set-num (arr ofs num)(let ((nbytes (>>(log2 num)3)))
"Записать в массив arr по смещению ofs число num"
  (progn (for i 0 nbytes
  (setq (aref arr (+ ofs i)(and (>> (* 8 i)num) 0xFF) )))
  (+ ofs nbytes))))

(defun arr-set-nums (arr ofs nums)(let ((adr ofs)(iter nums))
"Записать в массив arr по смещению ofs числа nums"
  (progn (for i 0 (length nums)
  (setq adr (arr-set-num arr adr (car iter)))
  (setq iter (cdr iter))))))

(defun reverse (lst)
"Реверс списка"
  (defun reverse1 (head tail)
    (if (null tail) head (reverse1 (cons (car tail) head) (cdr tail))))
    (reverse1 nil lst))

(defun struct-offsets(sizes ofs) (let ((offset ofs))
"Получение списка смещений,начиная с ofs,из описания структуры struct"
 (defun cons2 (lst x) (cons x lst))                        
 (reverse(cdr(foldl '(lambda (acc size)
    (cons2 acc (setq offset (+ offset size))))
                    (cons ofs nil) sizes)))))

(defun get-pairs(lst1 lst2 acc) (if (null lst1) acc
(get-pairs (cdr lst1) (cdr lst2)(cons(cons (car lst1)(cons (car lst2) nil))acc))))

(defun struct-fields (struct arr ofs)
   (car (foldl '(lambda (acc elem)
	    (let ((list (car acc))
		  (ofs (cdr acc))
		  (field (car elem))
		  (size (cdr elem)))
	      (cons (append list '(list `(,field (arr-get-num ,arr ,ofs ,size))))
		    (+ ofs size))))
	  (cons nil ofs) struct)))
					; (with-struct struct arr ofs body)
					; (let ((attr (arr-get-num arr ofs num))
					;       (type .. ))
					;         body)
(defmacro with-struct (struct arr ofs &rest body)
  "Выполнить вычисление body и установить значения переменных структуры struct из массива arr по смещению ofs"
  `(let ,(struct-fields struct arr ofs)) ,@body)

;(defmacro  with-struct-mbr (arr ofs &rest body)
;    `(let ((attr (arr-get-num arr ofs 1))
;	   (CHS-start (arr-get-num arr(+ ofs 1) 3))
;	   (type (arr-get-num arr (+ ofs 4) 1))
;	   (CHS-last (arr-get-num (+ ofs 5) 3)
;	   (start-sector (arr-get-num (+ ofs 8) 4))
;	   (num-sectors (arr-get-num (+ ofs 12) 4))))
;                ,body))

(setq arr #(0 0 1 0 0 1))
(setq num (arr-get-num arr 0 1))
(setq num0 (arr-get-num arr 0 1))
(setq num1 (arr-get-num arr 0 2))
(setq num2 (arr-get-num arr 0 3))
(setq num3 (arr-get-num arr 0 3))
(setq num4  (arr-get-num arr 0 4))
(set-struct-offsets partition-entry ofs)
(with-struct ((a.1)(b.2)(c.1)) arr 0
	     a)
					; ((a.1)(b.2)(c.1))
					; ((a (arr-get-num arr (+ ofs 0)))
					;  (b (arr-get-num arr (+ ofs 1)))
					;  (c (arr-get-num arr (+ ofs 3))))