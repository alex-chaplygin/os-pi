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

(defvar s '((a . 1)(b . 2)(c . 1)))
(defvar arr #(0 1 1 3))


(with-struct s arr (+ 0 0)
      a
      (+ a b c))
