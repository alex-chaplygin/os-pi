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
  (let ((acc 0))
    (progn 
      (for it 0 size 
	 (setq acc (+ acc it ))))acc));(<< (aref arr (+ ofs it)) (<< it 3))

(defun arr-get-nums (arr ofst struct)(let ((iter struct)(ofs ofst))
"Прочесть из массива arr по смещению ofs числа в байтах,указанных списком struct"
  (map (lambda (ofs)(setq ofs (arr-get-num arr ofs (car iter))) 
        (setq iter (cdr iter)) ) ofst)))

(defun arr-set-num (arr ofs num)(let ((nbytes (>>(log2 num)3)))
"Записать в массив arr по смещению ofs число num"
  (progn (for i 0 nbytes
  (setq (aref arr (+ ofs i)(and (>> (* 8 i)rem) 0xFF) )))
  (+ ofs nbytes))))

(defun arr-set-nums (arr ofs nums)(let ((adr ofs)(iter nums))
"Записать в массив arr по смещению ofs числа nums"
  (progn (for i 0 (length nums)
  (setq adr (arr-set-num arr adr (car iter)))
  (setq iter (cdr iter))))))


(defun reduce (f list init)(if (null list) init
"Левая свёртка функции f к списку list"
  (reduce f (cdr list)(funcall f init (car list)))))

(defun reduce-r (f list init)(if (null list) init
"Правая свёртка функции f к списку list"
  (reduce-r f (cdr list)(funcall f (car list) init))))

(defun reverse (lst)
"Реверс списка"
  (defun reverse1 (head tail)
    (if (null tail) head (reverse1 (cons (car tail) head) (cdr tail))))
    (reverse1 nil lst))

(defun struct-offsets (struct ofs) (let ((offset ofs))
"Получение списка смещений,начиная с ofs,из размеров полей struct"
 (reverse(cdr(reduce-r (lambda (size acc)
    (cons (setq offset (+ offset size)) acc))
      struct (list ofs))))))


					; (with-struct struct arr ofs body)
					; (let ((attr (make-int arr ofs num))
					;       (type .. ))
					;         body)

;(defun set-struct-values (struct arr ofs)
;  (let ((offset ofs)(struct-values struct))
;       (progn(for i 0 (length struct)
;	    (setq (nth i struct-values (list (car(nth i struct-values)) offset)))))))


(defmacro with-struct (struct arr ofs &rest body)
"Выполнить вычисление body и установить значения переменных структуры struct из массива arr по смещению ofs"
  `(let ,(arr-get-nums arr ofs struct) ,@body))

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
					;  (c (arr-get-num arr (+ of; Смещение таблицы разделов в MBR
(defvar +partition-table-ofs+ 0x1BE)

(defvar +FAT32-type+ 0x0C)

;;структура раздела FAT
(defvar partition-entry '((attributes . 1) ; атрибуты диска
			  (CHS-start . 3);адрес через номера цилиндра, головки и сектора
			  (type . 1); тип раздела
			  (CHS-last . 3)
			  (start-sector . 4)
			  (num-sectors . 4)))

(defvar s '((a . 1)(b . 2)(c . 1)))
(defvar arr #(0 1 1 3))


(with-struct s arr (+ 0 0)
      a
      (+ a b c))
