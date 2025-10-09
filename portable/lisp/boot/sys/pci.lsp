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

(defun pci-config-pack-address (bus device func offset)
  "Упаковывает PCI-адрес в 32-битное значение"
  (bitor +pci-enable-bit+
          (<< bus +pci-bus+)
          (<< device +pci-device+)
          (<< func +pci-function+)
          offset))

;; Чтение и запись данных из PCI
;; bus - номер шины
;; device - номер устройства
;; func - номер функции
;; offset - смещение адреса
(defun pci-config-read32 (bus device func offset)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (indw +pci-config-data+))

(defun pci-config-read16 (bus device func offset)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (inw +pci-config-data+))
  
(defun pci-config-read8 (bus device func offset)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (inb +pci-config-data+))

(defun pci-config-write32 (bus device func offset data)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (outdw +pci-config-data+ data))

(defun pci-config-write16 (bus device func offset data)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (outw +pci-config-data+ data))

(defun pci-config-write8 (bus device func offset data)
  (outdw +pci-config-address+ (pci-config-pack-address bus device func offset))
  (outb +pci-config-data+ data))
