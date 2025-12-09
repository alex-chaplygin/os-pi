; Драйвер ATA
(defconst +ata-data+ 0x1f0) ;регистр данных
(defconst +ata-error+ 0x1f1) ;регистр ошибок
(defconst +ata-n-sectors+ 0x1f2) ;регистр числа секторов
(defconst +ata-lba1+ 0x1f3) ;регистр LBA1
(defconst +ata-lba2+ 0x1f4) ;регистр LBA2
(defconst +ata-lba3+ 0x1f5) ;регистр LBA3
(defconst +ata-device+ 0x1f6) ;регистр номера устройства
(defconst +ata-status+ 0x1f7) ;регистр статуса (только чтение)
(defconst +ata-command+ 0x1f7) ;регистр комманд (только запись)

(defconst +ata-lba+ 0xE0) ;режим LBA для регистра устройства
(defconst +ata-cmd-read-sectors+ 0x20) ;команда чтения нескольких секторов
(defconst +ata-cmd-write-sectors+ 0x30) ;команда записи нескольких секторов
(defconst +sector-size+ 512) ;размер сектора

(defun ata-get-status-reg (bit)
  "Получение значения бита bit регистра статуса"
  (get-bit (inb +ata-status+) bit))

(defmacro mk/ata-wait-status (name bit val)
  "генерация функций ожидания бита регистра статуса"
  "name - имя, bit - номер бита статуса, val - значение для окончания ожидания"
  `(defun ,name () (if (= (ata-get-status-reg ,bit) ,val) nil (,name))))
  
(mk/ata-wait-status ata-wait 7 0) ; Ожидание контроллера: 1 - занят, 0 - свободен
(mk/ata-wait-status ata-wait-command-ready 6 1) ;Ожидание готовности к приему команд: 1 - готов
(mk/ata-wait-status ata-wait-data 3 1) ;Ожидание буфера данных: 1 - есть данные для чтения/записи

(defun ata-set-dev (dev)
  "Установка номера устройства"
  (outb +ata-device+ (bitor +ata-lba+ 0)));(<< dev 4)))) ;режим LBA + номер устройства

(defun ata-set-lba (sec num)
  "Установить номер сектора в режиме LBA"
  (outb +ata-n-sectors+ num) ;число секторов
  (outb +ata-lba1+ (& sec 0xff))
  (outb +ata-lba2+ (& (>> sec 8) 0xff))
  (outb +ata-lba3+ (& (>> sec 16) 0xff)))

(defun ata-check-error ()
  "Проверка ошибки"
  (cond ((= (ata-get-status-reg 0) 1) t)
	(t nil)))

(defun ata-set-command (cmd)
  "Установить команду контроллера"
  (outb +ata-command+ cmd))

(defun ata-read (size)
  "Чтение данных контроллера, size байт"
  (if (= size 0) #()
      (progn
	(ata-wait-data)
	(when (ata-check-error) (error "ATA read error"))
	(array-cat (insw +ata-data+ (>> +sector-size+ 1))
		   (ata-read (- size +sector-size+))))))

(defun ata-write (arr)
  "Запись массива arr в контроллер"
  (if (= arr #()) nil
      (progn
	(ata-wait-data)
	(when (ata-check-error) (error "ATA write error"))
	(outsw +ata-data+ (array-seq arr 0 +sector-size+))
	(ata-write (array-seq arr +sector-size+ (array-size arr))))))

(defun ata-identify ()
  "Чтение служебной информации: число головок, цилиндров, секторов,"
  "серийный номер, ..."
  (ata-wait)
  (ata-set-dev 0)
  (ata-wait-command-ready)
  (ata-set-command 0xec)
  (ata-read +sector-size+))

(defun ata-read-sectors (dev start num)
  "Читает сектора жесткого диска"
  "dev - устройство: 0 primary master, 1 - slave"
  "start - начальный сектор"
  "num - число секторов"
  "возвращает массив данных"  
  (if (= num 0) (error "invalid number of sectors")
      (progn
	 (ata-wait) ; ждем освобождения
	 (ata-set-dev dev)
	 (ata-wait-command-ready)
	 (ata-set-lba start num) ;установить стартовый сектор и количество
	 (ata-set-command +ata-cmd-read-sectors+)
	 (ata-read (* num +sector-size+)))))

(defun ata-write-sectors (dev start num arr)
  "Пишет сектора жесткого диска"
  "dev - устройство: 0 primary master, 1 - slave"
  "start - начальный сектор"
  "num - число секторов"
  "arr - массив байт данных"
    (if (= num 0) (error "invalid number of sectors")
	(progn
	 (ata-wait) ; ждем освобождения
	 (ata-set-dev dev)
	 (ata-wait-command-ready)
	 (ata-set-lba start num) ;установить стартовый сектор и количество
	 (ata-set-command +ata-cmd-write-sectors+)
	 (ata-write arr))))

(defun ata-test
    (ata-identify)
  (let ((sec (ata-read-sectors 0 0 1)))
    (for i 0 512 (seta sec i (& i 0xff)))
    (ata-write-sectors 0 1 1 sec)))
