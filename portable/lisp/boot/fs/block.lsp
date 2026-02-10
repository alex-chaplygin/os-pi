;; модуль для работы с блоками
(defvar *disk* 0) ; номер диска
(defvar *block-sectors* 1) ; число секторов в блоке
(defvar *block-size* 512) ; размер блока в байтах
(defvar *block-sector-offset* 0) ; смещение в секторах для отсчета блоков
(defvar *block-start-num* 0) ; значение минимального номера первого блока

(defun set-block-disk (disk)
  "Установить диск disk, с которого читать блоки"
  (unless (integerp disk) (throw 'error "set-block-disk: disk must be integer"))
  (setq *disk* disk))

(defun set-block-size (size)
  "Установить размер блока диска size в секторах"
  (unless (integerp size) (throw 'error "set-block-size: size must be integer"))
  (setq *block-sectors* size)
  (setq *block-size* (* size +sector-size+)))

(defun set-block-offset (offset)
  "Установить смещение в секторах для отсчета блоков диска"
  (unless (integerp offset) (throw 'error "set-block-offset: offset must be integer"))
  (setq *block-sector-offset* offset))

(defun set-blocks-start-num (start-num)
  "Установить значение минимального номера первого блока"
  (unless (integerp start-num) (throw 'error "set-blocks-start-num: offset must be integer"))
  (setq *block-start-num* start-num))

(defun block-read (num)
  "Прочитать блок с номером num"
  (when (< num *block-start-num*) (throw 'error "block-read: block num cant be less the block-start-num"))
  (let* ((sec (+ *block-sector-offset* (* num *block-sectors*))))
    (ata-read-sectors *disk* sec *block-sectors*)))

(defun block-write (num buf)
  (when (< num *block-start-num*) (throw 'error "block-write: block num cant be less the block-start-num"))
  "Записать буфер buf в блок с номером num"
  (ata-write-sectors *disk* (+ *block-sector-offset* (* num *block-sectors*)) *block-sectors* buf))

(defun get-blocks-pos (blocks offset)
  "Получить индекс блока и смещение по смещению offset в списке блоков blocks"
  (let ((pos nil)
        (len (list-length blocks)))
    (when (> offset (* len *block-size*)) (throw 'error "get-blocks-pos: position outside of blocks"))
    (for i 0 (+ len 1)
         (if (< offset *block-size*) (setq pos (cons i offset)
					   i len)
             (setq offset (- offset *block-size*)
		   blocks (cdr blocks))))
    pos))

(defun get-offset-from-pos (pos)
  "Получить смещение в байтах из позиции(индекс блока . смещение в блоке)"
  (+ (* (car pos) *block-size*) (cdr pos)))
