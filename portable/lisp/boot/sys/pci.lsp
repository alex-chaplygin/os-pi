;; Драйвер шины PCI
;; Шина PCI предоставляет возможность программной конфигурации любого устройства 
;; через специальное адресное пространство
;; Для этого каждое устройство предоставляет 256 байт регистров конфигурации

(defconst +pci-config-address+ 0xCF8) ; порт адреса конфигурации
(defconst +pci-config-data+ 0xCFC) ; порт данных конфигурации
(defconst +pci-enable-bit+ 0x80000000); бит доступа к pci
(defconst +pci-bus+ 16); сдвиг для поля bus
(defconst +pci-device+ 11); сдвиг для поля device
(defconst +pci-function+ 8); сдвиг для поля function
(defconst +pci-config-class+ 8); смещение регистра с Class/Subclass
(defconst +pci-config-bar0+ 0x10); смещение регистра BAR 0
(defconst +pci-config-vendor+ 0); смещение для Vendor ID
(defconst +pci-config-command+ 4) ; смещение регистра command
;; Флаги регистра команд PCI
(defconst +pci-command-io+ 1) ; доступ к I/O адресам устройства
(defconst +pci-command-mem+ (<< 1 1)) ; доступ к памяти устройства
(defconst +pci-command-bus+ (<< 1 2)) ; устройство работает как главное на шине (обязательно для DMA)

(defun pci-config-pack-address (bus device func)
  "Упаковывает PCI-адрес в 32-битное значение"
  (bitor +pci-enable-bit+
          (<< bus +pci-bus+)
          (<< device +pci-device+)
          (<< func +pci-function+)))

;; Чтение и запись данных из PCI
;; bus - номер шины
;; device - номер устройства
;; func - номер функции
;; offset - смещение адреса
(defmacro gen-read/pci (name port-func)
  `(defun ,name (pci offset)
     (outdw +pci-config-address+ (+ pci offset))
     (,port-func +pci-config-data+)))

(gen-read/pci pci-config-read32 indw)
(gen-read/pci pci-config-read16 inw)
(gen-read/pci pci-config-read8 inb)

(defmacro gen-write/pci (name port-func)
  `(defun ,name (pci offset data)
     (outdw +pci-config-address+ (+ pci offset))
     (,port-func +pci-config-data+ data)))

(gen-write/pci pci-config-write32 outdw)
(gen-write/pci pci-config-write16 outw)
(gen-write/pci pci-config-write8 outb)

(defun get-pci-class (pci)
  "Функция читает Class code и Subclass устройства PCI, возвращает пару (class . subclass)"
  (let ((val (pci-config-read32 pci +pci-config-class+)))
    (cons (& (>> val 24) 0xff)   ; Class
          (& (>> val 16) 0xff)))) ; Subclass

(defun get-pci-bar (pci index)
  "Функция читает BAR по индексу и возвращает его адрес"
  (let ((bar (pci-config-read32 pci (+ +pci-config-bar0+ (<< index 2)))))
   (& bar (- 0xffffffff (if (& bar 1) 0x3 0xF)))))

(defun get-pci-vendor-device (pci)
  "Функция читает Vendor ID b Device ID, возвращает пару (vendor . device)"
  (let ((id (pci-config-read32 pci +pci-config-vendor+)))
    (cons (& id 0xffff) ; Vendor ID
	  (& (>> id 16) 0xffff)))) ; Device ID

(defun pci-set-command-flags (pci flags enable)
  "Установить (enable=t) или сбросить флаги flags регистра команд PCI устройства pci"
  (let ((command (pci-config-read16 pci +pci-config-command+)))
    (pci-config-write16 pci +pci-config-command+ (if enable (bitor command flags)
						     (& command (- 0xffff flags))))))

(defun pci-set-mem-enable (pci enable)
  "Установить (enable=t) или сбросить доступ ко всей памяти устройства устройства pci"
  (pci-set-command-flags pci (+ +pci-command-io+ +pci-command-mem+ +pci-command-bus+) enable))
