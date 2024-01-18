; Смещение таблицы разделов в MBR
(defvar sec0(ata-read-sectors 0 0 1))
(defvar +mbr-begin-ofs+ 13)
(defvar +partition-table-ofs+ 0x1BE)
(defvar +partition-rec-size+ 16)
(defvar +FAT32-type+ 0x0C)
(defvar +sectors-per-cluster+ (arr-get-num sec0 13 1))
(defvar +reserved-sectors+ (arr-get-num sec0 14 2))
(defvar +Fat-structs+ (arr-get-num sec0 16 1))
(defvar +fat32-struct-sectors+ (arr-get-num sec0 36 4))
(defvar +root-cluster+ (arr-get-num sec0 44 4))
(defvar +fsinfo-sector+ (arr-get-num sec0 48 2))
(defvar +fsi-struc-ofs+ 484)
(defun fs-info-sec(dev) (ata-read-sectors dev 1 1))
(defvar fsinfo-sec (fs-info-sec 1))
(defvar +fsi-struc-sig+ 0x41615252)
(defvar +fsi-struc-sig2-ofs+ 484)
(defvar +fsi-struc-sig2+ 0x61417272)
(defvar +fsi-struc-sig3-ofs+ 508)
(defvar +fsi-struc-sig3+ 0xAA550000)
(defvar +fsi-free-clusters-ofs+ 488)
(defvar +fsi-next-free-cluster-ofs+ 492)
;;Структура глобальных параметров MBR
(defvar mbr-struct '(;(mbr-skip0 . 13)
                     (sectors-per-cluster . 1)
                     (reserved-sectors . 2)
                     (Fat-structs . 1)
                     (mbr-skip1 . 19)
                     (fat32-struct-sectors . 4)
                     (root-cluster . 4))
                     (fsinfo-sector . 2))
;;структура раздела FAT
(defvar partition-entry '((attributes . 1) ; атрибуты диска
			  (CHS-start . 3);адрес через номера цилиндра, головки и сектора
			  (type . 1); тип раздела
			  (CHS-last . 3)
			  (start-sector . 4)
			  (num-sectors . 4)))


(defun show-mbr-info ()
  (defvar sec0(ata-read-sectors 0 0 1))
  (eval (with-struct partition-entry sec0 +partition-table-ofs+
  '(- +FAT32-type+ (aref sec0 (+ +partition-table-ofs+ 4)))
  '(list (cons 'CHS-start CHS-start) (cons 'type  type) (cons 'CHS-last CHS-last) (cons 'start-sector  start-sector) (cons 'num-sectors  num-sectors)))))

;Чтение disk.qcow2; 
(defun load-partition (disk-num part-num)
  (let ((sec (ata-read-sectors disk-num 0 1)) (sec2 (ata-read-sectors disk-num +fsinfo-sector+ 1)))
  (with-struct mbr-struct sec +mbr-begin-ofs+ 
	(with-struct partition-entry sec (+ (* +partition-rec-size+ part-num) +partition-table-ofs+)
		'(case type
			(+FAT32-type+ 
			(case (arr-get-num sec2 0)
				(+fsi-struc-sig+
			(case (arr-get-num sec2 +fsi-struc-sig2-ofs+)
				(+fsi-struc-sig2-sig+
			(load-fat32 (+ start-sector reserved-sectors) num-sectors Fat-structs fat32-struct-sectors sectors-per-cluster )))))))))))

(defun load-fat32 (start-sector num-sectors Fat-structs fat32-struct-sectors sectors-per-cluster) 
	(let ((fat32-struct-size (* fat32-struct-sectors Fat-structs))(fat-structs  (ata-read-sectors disk-num start-sector fat32-struct-size))
	(fat32-data (ata-read-sectors disk-num (- num-sectors fat32-struct-size)))) 
	))
(cons '+sectors-per-cluster+ +sectors-per-cluster+)
(cons '+reserved-sectors+ +reserved-sectors+)
(cons '+Fat-structs+ +Fat-structs+)
(cons '+fat32-struct-sectors+ +fat32-struct-sectors+)
(cons '+root-cluster+ +root-cluster+)
(cons '+fsinfo-sector+ +fsinfo-sector+)
;(show-mbr-info)

(load-partition 0 0)
