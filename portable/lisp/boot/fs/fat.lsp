; модуль для работы с FAT
(defvar *fat*) ; хеш-объект FAT
(defvar *fat-start-sector*) ; начальный сектор FAT
(defvar *fat-sectors*) ; всего секторов в FAT
; типы записей в FAT
(defconst +free-block+ 0) ; свободный блок
(defconst +max-block+ 0xfffffef) ; максимальный номер блока
(defconst +reserved-block-start+ 0xffffff0) ; зарезервированные блоки 
(defconst +reserved-block-end+ 0xffffff6) ; 
(defconst +bad-block+ 0xffffff7) ; поврежденный блок
(defconst +end-block-start+ 0xffffff8) ; метки окончания цепочки
(defconst +end-block-end+ 0xfffffff) ; 

(defun get-fat-chain (start)
  "Загрузить цепочку блоков из FAT, начиная с start"
  "Обновить глобальный объект FAT, если такой цепочки там нет"
  (if (check-key *fat* start) (cons start (get-hash *fat* start))
      (cons start (set-hash *fat* start (get-fat-chain* start)))))

(defun get-fat-chain* (cur)
  "Рекурсивная загрузка цепочки FAT"
  (let ((el (read-fat cur)))
    (if (and (>= el +end-block-start+) (<= el +end-block-end+)) nil
	(cons el (get-fat-chain* el)))))

(defun read-fat (num)
  "Читает запись FAT с номером num"
  ;(if (> num +max-block+) '(error "read-fat: invalid block number")
      (let ((sec (+ *fat-start-sector* (>> num 7))) ; номер сектора
	    (ofs (<< (& num 0x7f) 2))) ;смещение внутри сектора
	(& 0xfffffff (arr-get-num (ata-read-sectors *disk* sec 1) ofs 4))));)
