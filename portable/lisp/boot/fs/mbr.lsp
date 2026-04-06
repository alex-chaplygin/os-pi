(defconst +partition-table-ofs+ 0x1BE) ;; Смещение таблицы разделов в MBR(446 байт)
(defconst +partition-rec-size+ 16) ;; Размер записи раздела
(defconst +partition-active+ 128) ;; Атрибут диска означающий что раздел диска занят
;; Тип файловой системы
(defconst +FAT32-type+ 0x0C) ;; FAT32
(defconst +CDFS-type+ 0x94) ;; ISO9660(CDFS)

(defun parse-partition-rec ()
  "Парсер записи таблицы раздеров в MBR"
  (&&&
   part-rec->(parse-struct '((attributes . 1) ;; атрибуты диска
			     (CHS-start . 3) ;; адрес через номера цилиндра, головки и сектора
			     (type . 1) ;; тип раздела
			     (CHS-last . 3)
			     (start-sector . 4) ;; начальный сектор
			     (num-sectors . 4))) ;; количество секторов
   return
   (progn
     (let ((start-sector (arr-get-num (get-hash part-rec 'start-sector) 0 4))
           (sector-count (arr-get-num (get-hash part-rec 'num-sectors) 0 4))
           (fs-type (arr-get-num (get-hash part-rec 'type) 0 1))
           (part-active (arr-get-num (get-hash part-rec 'attributes) 0 1)))
       (unless (= part-active +partition-active+) (print part-active) (raise 'error "parse-partition-rec: mbr partition is not active"))
       (case fs-type
         (+FAT32-type+ (setq *file-system* (make-instance 'FAT32FileSystem)))
         (+CDFS-type+ (setq *file-system* (make-instance 'CDFSFileSystem)))
         (otherwise (raise 'error "parse-partition-rec: mbr partition file system type unknown")))
       (fs-init *file-system* *disk* start-sector sector-count)))))

(defun load-partition (disk-num part-num)
  "Загрузить файловую систему из раздела part-num с диска disk-num"
  (when (or (< part-num 0) (> part-num 3)) (raise 'argument-error "load-partition: part-num must be in range [0;3]"))
  (set-block-disk disk-num)
  (let ((partition-offset (+ +partition-table-ofs+ (* +partition-rec-size+ part-num)))
        (mbr-sector (ata-read-sectors disk-num 0 1)))
    (car (funcall
          (parse-partition-rec)
	  (stream-from-arr (array-seq mbr-sector partition-offset (+ partition-offset +partition-rec-size+)) nil)))))

;; (load-partition 0 0)
