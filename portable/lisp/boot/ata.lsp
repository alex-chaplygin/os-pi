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
(defvar +sector-size+ 512) ;размер сектора

(defun ata-get-status-reg (bit)
  "Получение значения бита bit регистра статуса"
  (get-bit (inb +ata-status+) bit))

(defmacro ata-status (name bit) ;генерация функций для регистра статуса
  `(defun ,name () (ata-get-status-reg ,bit)))
  
(ata-status ata-get-busy 7) ; статус: 1 - занят, 0 - свободен
(ata-status ata-get-command-ready 6) ;готовность к приему команд: 1 - готов
(ata-status ata-has-data 3) ;статус данных: 1 - есть данные для считывания

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
  (outb +ata-lba1+ sec 0xff)
  (outb +ata-lba2+ (& (>> sec 8) 0xff))
  (outb +ata-lba3+ (& (>> sec 16) 0xff)))

(defun ata-check-error ()
  "Проверка ошибки"
  (cond ((= (ata-get-status-reg 0) 1) t)
	(t nil)))

(defun ata-set-command (cmd)
  "Установить команду контроллера"
  (outb +ata-command+ cmd))

(defun ata-read-old (arr i)
  "Чтение данных жесткого диска и запись в массив, начиная с позиции i"
  (ata-wait)
  (if (ata-check-error) '(read error)
      (let ((d (inw +ata-data+)))
	(progn
	   (seta arr i (& d 0xff))
	   (seta arr (++ i) (>> d 8))
	   (if (= (ata-has-data) 0) arr
	       (ata-read arr (+ i 2)))))))

(defun ata-read (size)
  "Чтение данных жесткого диска, size байт"
  (ata-wait)
  (if (ata-check-error) '(read error)
      (insw +ata-data+ (>> size 1))))

(defun ata-write (arr)
  "Чтение данных жесткого диска, size байт"
  (ata-wait)
  (if (ata-check-error) '(read error)
      (outsw +ata-data+ arr)))

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
  (if (= num 0) '(invalid number of sectors)
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
    (if (= num 0) '(invalid number of sectors)
	(progn
	 (ata-wait) ; ждем освобождения
	 (ata-set-dev dev)
	 (ata-wait-command-ready)
	 (ata-set-lba start num) ;установить стартовый сектор и количество
	 (ata-set-command +ata-cmd-write-sectors+)
	 (ata-write arr))))

(defun ata-test
    (ata-identify)
  (defvar sec (ata-read-sectors 0 0 1))
  (for i 0 512 (seta sec i (& i 0xff)))
  (ata-write-sectors 0 1 1 sec))
