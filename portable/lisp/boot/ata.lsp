; Драйвер ATA

(defvar +ata-data+ 0x1f0) ;регистр данных
(defvar +ata-error+ 0x1f1) ;регистр ошибок
(defvar +ata-n-sectors+ 0x1f2) ;регистр числа секторов
(defvar +ata-lba1+ 0x1f3) ;регистр LBA1
(defvar +ata-lba2+ 0x1f4) ;регистр LBA2
(defvar +ata-lba3+ 0x1f5) ;регистр LBA3
(defvar +ata-device+ 0x1f6) ;регистр номера устройства
(defvar +ata-status+ 0x1f7) ;регистр статуса (только чтение)
(defvar +ata-command+ 0x1f7) ;регистр комманд (только запись)

(defvar +ata-lba+ 0xE0) ;режим LBA для регистра устройства
(defvar +ata-cmd-read-sectors+ 0x20) ;команда чтения нескольких секторов
(defvar +ata-cmd-write-sectors+ 0x30) ;команда записи нескольких секторов

(defun ata-get-status-reg (bit)
  "Получение значения бита bit регистра статуса"
  (get-bit (inb +ata-status+) bit))

(defun ata-get-busy ()
  "Получение статуса: 1 - занят, 0 - свободен"
  (ata-get-status-reg 7))

(defun ata-get-command-ready ()
  "Получение готовности к приему команд: 1 - готов, 0 - нет"
  (ata-get-status-reg 6))

(defun ata-has-data ()
  "Получение статуса данных: 1 - есть данные для считывания, 0 - нет"
  (ata-get-status-reg 3))

(defun ata-wait ()
  "Ожидание пока не освободится контроллер"
  (cond
    ((= (ata-get-busy) 0) nil)
    (t (ata-wait))))

(defun ata-wait-command-ready ()
  "Ожидание пока ATA не готов к приему команд"
  (cond
    ((= (ata-get-command-ready) 1) nil)
    (t (ata-wait-command-ready))))

(defun ata-set-dev (dev)
  "Установка номера устройства"
  (outb +ata-device+ (bitor +ata-lba+ 0)));(<< dev 4)))) ;режим LBA + номер устройства

(defun ata-set-lba (sec num)
  "Установить номер сектора в режиме LBA"
  (outb +ata-n-sectors+ num) ;число секторов
  (outb +ata-lba1+ sec 0xff))
  (outb +ata-lba2+ (& (>> sec 8) 0xff))
  (outb +ata-lba3+ (& (>> sec 16) 0xff)))

(defun ata-check-error ()
  "Проверка ошибки"
  (cond ((= (ata-get-status-reg 0) 1) t)
	(t nil)))

(defun ata-set-command (cmd)
  "Установить команду контроллера"
  (outb +ata-command+ cmd))

(defun read-store (arr pos)
  "Чтение и запись данных в массив в позиции pos"
  (ata-wait)
	     (seta arr pos (& (inw +ata-data+) 0xff))
	     (cond
	       ((= pos 19) arr);(ata-has-data) 0) arr)
	       (t (read-store arr (+ pos 1)))))

(defun write-store (arr pos)
  "Запись данных в жесткий диск"
  (ata-wait)
  (outw +ata-data+ (aref arr pos))
  (cond
    ((= pos 511) arr);(ata-has-data) 0) arr)
    (t (write-store arr (+ pos 1)))))

(defun ata-identify ()
  (ata-wait)
  (ata-set-dev 0)
  (ata-wait-command-ready)
  (ata-set-command 0xec)
  (ata-wait)
  (make-array 'res 10)
  (seta res 0 (inw +ata-data+))
  (seta res 1 (inw +ata-data+))
  (seta res 2 (inw +ata-data+))
  (seta res 3 (inw +ata-data+))
  (seta res 4 (inw +ata-data+))
  (seta res 5 (inw +ata-data+))
  (seta res 6 (inw +ata-data+))
  res)

(defun ata-read-sectors (dev start num)
  "Читает сектора жесткого диска"
  "dev - устройство: 0 primary master, 1 - slave"
  "start - начальный сектор"
  "num - число секторов"
  "возвращает массив данных"  
  (cond
    ((= num 0) '(invalid number of sectors))
    (t (progn
	 (ata-wait) ; ждем освобождения
	 (ata-set-dev dev)
	 (ata-get-command-ready)
	 (ata-set-lba start num) ;установить стартовый сектор и количество
	 (ata-set-command +ata-cmd-read-sectors+)
	 (read-store (make-array 'a 20) 0)))))

(defun ata-write-sectors (dev start num arr)
  "Пишет сектора жесткого диска"
  "dev - устройство: 0 primary master, 1 - slave"
  "start - начальный сектор"
  "num - число секторов"
  (cond
    ((= num 0) '(invalid number of sectors))
    (t (progn
	 (ata-wait) ; ждем освобождения
	 (ata-set-dev dev)
	 (ata-get-command-ready)
	 (ata-set-lba start num) ;установить стартовый сектор и количество
	 (ata-set-command +ata-cmd-write-sectors+)
	 (write-store arr 0)))))

;(ata-identify)
(ata-read-sectors 0 1 1)
