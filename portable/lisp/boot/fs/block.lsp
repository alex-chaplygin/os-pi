; модуль для работы с блоками
(defvar *disk* 0) ; номер диска
(defvar *block-sectors* 1) ; число секторов в блоке
(defvar *block-size* 512) ; размер блока в байтах
(defvar *block-sector-offset* 0) ; смещение в секторах для отсчета блоков

(defun block-read (num)
  "Прочитать блок с номером num"
  (let* (;(b (make-array *block-size*))
	 (sec (+ *block-sector-offset* (* num *block-sectors*))))
	 ;(b1 (ata-read-sectors *disk* sec 1))
	 ;(b2 (ata-read-sectors *disk* (+ sec 1) 1)))
;    (for s 0 *block-sectors*
;	 (let ((b1 (ata-read-sectors *disk* (+ sec s) 1)))
;	   (for i 0 512 (seta b (+ i (* 512 s)) (aref b1 i)))))
					;    b))
    (ata-read-sectors *disk* sec *block-sectors*)))

(defun block-write (num buf)
  "Записать буфер buf в блок с номером num"
  (ata-write-sectors *disk* (+ *block-sector-offset* (* num *block-sectors*)) *block-sectors* buf))
