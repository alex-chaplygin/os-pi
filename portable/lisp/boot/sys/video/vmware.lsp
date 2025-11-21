;; Версии устройства
(defconst +svga-id-0+ 0)
(defconst +svga-id-1+ 1)
(defconst +svga-id-2+ 2)
;; Регистры VMWARE
(defconst +svga-reg-id+  0)
(defconst +svga-reg-enable+  1)
(defconst +svga-reg-width+  2)
(defconst +svga-reg-height+  3)
(defconst +svga-reg-max-width+  4)
(defconst +svga-reg-max-height+  5)
(defconst +svga-reg-depth+  6)
(defconst +svga-reg-id+ 15) ; Размер VRAM
(defconst +svga-reg-vram-size+ 15) ; Размер VRAM
(defconst +svga-reg-fb-size+ 16) ; Размер Framebuffer
(defconst +svga-reg-mem-size+ 19) ; Размер FIFO (MEM-SIZE)
;; Глобальные переменные SVGA
(defvar *svga-io-base*) ;; Базовый адрес портов памяти регистров
(defvar *svga-fb-base*) ;; Адрес ОЗУ буфера кадра
(defvar *svga-fifo-base*) ;; Адрес ОЗУ очереди команд
(defvar *svga-vram-size*) ;; Размер видеопамяти
(defvar *svga-fb-size*) ;; Размер буфера кадра
(defvar *svga-fifo-size*) ;; Размер очереди команд

(defvar *svga-pci*) ;; PCI устройство


;; Чтение/запись регистров
(defun svga-write-reg (reg value)
  "Записать значение в регистр SVGA"
  (outdw (+ *svga-io-base* (<< reg 2)) value))

(defun svga-read-reg (reg)
  "Прочитать значение из регистра SVGA"
  (indw (+ *svga-io-base* (<< reg 2))))

;; Инициализация SVGA 
(defun svga-init (bus device)
  "Включает доступ к памяти, получает базовые адреса SVGA устройства"
  ;; Создаем PCI адрес из bus/device
  (setq *svga-pci* (pci-config-pack-address bus device 0))  
    ;; Включаем память устройства
  (pci-set-mem-enable *svga-pci* t)
  ;; Получаем базовые адреса в глобальные переменные
  (setq *svga-io-base* (get-pci-bar *svga-pci* 0))
  (setq *svga-fb-base* (get-pci-bar *svga-pci* 1)) 
  (setq *svga-fifo-base* (get-pci-bar *svga-pci* 2))
;;  (svga-write-reg +svga-reg-enable+ 1)
  (svga-write-reg +svga-reg-id+ (+ (<< 0x900000 8) +svga-id-2+))
  (setq *svga-vram-size* (svga-read-reg +svga-reg-vram-size+))
  (setq *svga-fb-size* (svga-read-reg +svga-reg-fb-size+))
  (setq *svga-fifo-size* (svga-read-reg +svga-reg-mem-size+)))
