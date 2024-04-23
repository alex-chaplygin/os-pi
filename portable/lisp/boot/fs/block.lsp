; модуль для работы с блоками
(defvar *disk* 0) ; номер диска
(defvar *block-sectors* 1) ; число секторов в блоке
(defvar *block-size* 512) ; размер блока в байтах
(defvar *block-sector-offset* 0) ; смещение в секторах для отсчета блоков

(defun block-read (num)
  "Прочитать блок с номером num"
  (ata-read-sectors *disk* (+ *block-sector-offset* (* num *block-sectors*)) *block-sectors*))

(defun block-write (num buf)
  "Записать буфер buf в блок с номером num"
  (ata-write-sectors *disk* (+ *block-sector-offset* (* num *block-sectors*)) *block-sectors* buf))
