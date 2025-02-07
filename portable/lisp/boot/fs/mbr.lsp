(defvar +partition-table-ofs+ 0x1BE) ; Смещение таблицы разделов в MBR
(defvar +partition-rec-size+ 16) ; Размер записи раздела
; Тип файловой системы
(defvar +FAT32-type+ 0x0C)

;;структура записи раздела
(defvar partition-entry '((attributes . 1) ; атрибуты диска
			  (CHS-start . 3);адрес через номера цилиндра, головки и сектора
			  (type . 1); тип раздела
			  (CHS-last . 3)
			  (start-sector . 4) ; начальный сектор
			  (num-sectors . 4))) ; число секторов

(defun load-partition (disk-num part-num)
  "Загрузить файловую систему из раздела part-num с диска disk-num"
  (let ((sec (ata-read-sectors disk-num 0 1))
	(part-ofs (+ (* +partition-rec-size+ part-num) +partition-table-ofs+)))
    (with-struct partition-entry sec part-ofs
      (case type
	(+FAT32-type+ (setq *file-system* (make-instance Fat32FileSystem))))
      (init *file-system* disk-num start-sector num-sectors))))

(load-partition 0 0)
