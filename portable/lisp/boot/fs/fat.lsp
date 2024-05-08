; модуль для работы с FAT
(defvar *fat*) ; хеш-объект FAT
(defvar *fat-start-sector*) ; начальный сектор FAT
(defvar *fat-sectors*) ; всего секторов в FAT
(defvar *free-blocks-count*) ; число свободных блоков
(defvar *last-free-block*)  ; последний свободный блок
; типы записей в FAT
(defconst +free-block+ 0) ; свободный блок
(defconst +max-block+ 0xfffffef) ; максимальный номер блока
(defconst +reserved-block-start+ 0xffffff0) ; зарезервированные блоки 
(defconst +reserved-block-end+ 0xffffff6) ; 
(defconst +bad-block+ 0xffffff7) ; поврежденный блок
(defconst +end-block-start+ 0xffffff8) ; метки окончания цепочки
(defconst +end-block-end+ 0xfffffff) ;
(defconst +free-blocks-count-pos+ 0x1e8) ;позиция в FSInfo
(defconst +last-free-block-pos+ 0x1ec) ;позиция в FSInfo

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

(defun get-fat-pos (num)
  "Возвращает пару (сектор.смещение) для блока с номером num"
  (cons (+ *fat-start-sector* (>> num 7)) (<< (& num 0x7f) 2)))

(defun read-fat (num)
  "Читает запись FAT с номером num"
  ;(if (> num +max-block+) '(error "read-fat: invalid block number")
  (let* ((p (get-fat-pos num))
	 (sec (car p)) ; номер сектора
	 (ofs (cdr p))) ;смещение внутри сектора
	(& 0xfffffff (arr-get-num (ata-read-sectors *disk* sec 1) ofs 4))));)

(defun fat-get-free-block ()
  "Найти первый свободный блок"
  (fat-get-free-block* (if (= *last-free-block* -1) 2 *last-free-block*)))
(defun fat-get-free-block* (pos)
  (let ((b (read-fat pos)))
    (if (= b +free-block+) pos (fat-get-free-block* (+ pos 1)))))

(defun update-fat (num val)
  "Записать значение val в запись с номером num у 2-х копий FAT"
  (let* ((p (get-fat-pos num))
	 (sec-num (car p)) ; номер сектора
	 (ofs (cdr p))
	 (sec (ata-read-sectors *disk* sec-num 1))) ;смещение внутри сектора
    (arr-set-num sec ofs val 4)
    (ata-write-sectors *disk* sec-num 1 sec)
    (ata-write-sectors *disk* (+ sec-num *fat-sectors*) 1 sec)))

(defun fat-append-chain (start bl)
  "Добавить новый блок bl в цепочку, начиная со start"
  (let ((chain (get-fat-chain start)))
    (update-fat (last chain) bl)
    (update-fat bl +end-block-end+) ;занимаем свободный блок
    (update-last-free-block (fat-get-free-block)) ;ищем новый свободный
    (update-free-blocks-count (-- *free-blocks-count*)) ;уменьшаем число свободных блоков
    (set-hash *fat* start (append (cdr chain) (list bl)))))

(defmacro mk/update-fs (name var pos)
  `(defun ,name (b)
     (let ((fs (ata-read-sectors *disk* *fs-info-sec* 1)))
       (setq ,var b)
       (arr-set-num fs ,pos b 4)
       (ata-write-sectors *disk* *fs-info-sec* 1 fs))))
;Поменять последний свободный блок в FSInfo
(mk/update-fs update-last-free-block *last-free-block* +last-free-block-pos+)
;Поменять число свободных блоков
(mk/update-fs update-free-blocks-count *free-blocks-count* +free-blocks-count-pos+)
