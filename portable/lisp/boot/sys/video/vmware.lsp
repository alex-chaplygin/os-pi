;; Версии устройства
(defconst +svga-id-0+ 0)
(defconst +svga-id-1+ 1)
(defconst +svga-id-2+ 2)
(defconst +svga-magic+ 0x900000); уникальная подпись VMware для формирования ID устройства
;; Смещения относительно BAR0
(defconst +svga-index-port+ 0)
(defconst +svga-value-port+ 1)
;; Регистры VMWARE
(defconst +svga-reg-id+  0)
(defconst +svga-reg-enable+  1)
(defconst +svga-reg-width+  2)
(defconst +svga-reg-height+  3)
(defconst +svga-reg-max-width+  4)
(defconst +svga-reg-max-height+  5)
(defconst +svga-reg-depth+  6)
(defconst +svga-reg-bits-per-pixel+ 7); отвечает за глубину цвета (BPP)
(defconst +svga-reg-bytes-per-line+ 12); хранит Pitch (шаг строки)
(defconst +svga-reg-vram-size+ 15) ; Размер VRAM
(defconst +svga-reg-fb-size+ 16) ; Размер Framebuffer
(defconst +svga-reg-mem-size+ 19) ; Размер FIFO (MEM-SIZE)
(defconst +svga-reg-config-done+ 20) ; Размер FIFO (MEM-SIZE)
(defconst +svga-reg-sync+ 21) ;; Регистр для синхронизации
(defconst +svga-reg-busy+ 22)
(defconst +svga-fifo-num-regs+ 291) ; Количество регистров в заголовке FIFO
;; Коды команд FIFO
(defconst +svga-cmd-update+ 1)

(defconst +svga-fifo-next-offset+ 8)

;; Глобальные переменные SVGA
(defvar *svga-io-base*) ;; Базовый адрес портов памяти регистров
(defvar *svga-fb-base*) ;; Адрес ОЗУ буфера кадра
(defvar *svga-fifo-base*) ;; Адрес ОЗУ очереди команд
(defvar *svga-vram-size*) ;; Размер видеопамяти
(defvar *svga-fb-size*) ;; Размер буфера кадра
(defvar *svga-fifo-size*) ;; Размер очереди команд
(defvar *svga-pitch*) ;; длина строки экрана в байтах
(defvar *svga-pci*) ;; PCI устройство
;; Переменная для хранения текущей позиции записи в FIFO
(defvar *svga-fifo-cursor*)
;; Переменная для хранения начала FIFO
(defvar *svga-fifo-start*)

;; Чтение/запись регистров
(defun svga-write-reg (reg value)
  "Записать значение в регистр SVGA"
  (outdw (+ *svga-io-base* +svga-index-port+) reg)
  (outdw (+ *svga-io-base* +svga-value-port+) value))

(defun svga-read-reg (reg)
  "Прочитать значение из регистра SVGA"
  (outdw (+ *svga-io-base* +svga-index-port+) reg)
  (indw (+ *svga-io-base* +svga-value-port+)))

(defun svga-sync ()
  "Ждет завершения всех операций видеокарты"
  ;; 1. Пишем 1 в SYNC (начать синхронизацию)
  (svga-write-reg +svga-reg-sync+ 1)
  ;; 2. Ждем пока BUSY не станет 0
  (while (!= (svga-read-reg +svga-reg-busy+) 0)
  	 nil))

;; Инициализация FIFO
(defun svga-init-fifo ()
  "Инициализация очереди команд (FIFO) через OStream"
  (let ((s (new-stream))
        ;; Отступ в байтах: 291 * 4 = 1164 байта
        (min-offset (<< +svga-fifo-num-regs+ 2)))
    ;;Запоминаем, что писать начнем с min-offset
    (setq *svga-fifo-cursor* min-offset *svga-fifo-start* min-offset) 
    ; Записываем 4 значения заголовка
    (write-dword s min-offset);; MIN
    (write-dword s *svga-fifo-size*) ;; MAX: размер памяти FIFO
    (write-dword s min-offset) ;; NEXT: писать сюда (в начало)
    (write-dword s min-offset) ;; STOP: читать до сюда (в начало)
    (print `(fifo ,min-offset ,(ostream-data s)))
    (memcpy *svga-fifo-base* (ostream-data s))))

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
  ;(let ((id-2 (+ (<< +svga-magic+ 8) +svga-id-2+)))
   ; (svga-write-reg +svga-reg-id+ id-2))
  (setq *svga-vram-size* (svga-read-reg +svga-reg-vram-size+))
  (setq *svga-fb-size* (svga-read-reg +svga-reg-fb-size+))
  (setq *svga-fifo-size* (svga-read-reg +svga-reg-mem-size+))
  (svga-init-fifo) 
  (svga-write-reg +svga-reg-config-done+ 1))

(defun svga-set-mode (width height bpp)
  "Устанавливает разрешение и глубину цвета"
  (svga-write-reg +svga-reg-width+ width)
  (svga-write-reg +svga-reg-height+ height)
  (svga-write-reg +svga-reg-bits-per-pixel+ bpp)
  ;; Включаем SVGA режим (1 = TRUE)
  (svga-write-reg +svga-reg-enable+ 1)
  ;; Считываем реальную длину строки в байтах. 
  (setq *svga-pitch* (svga-read-reg +svga-reg-bytes-per-line+)))

(defun svga-command (c)
  "Послать команду в очередь"
  (memcpy (+ *svga-fifo-base* *svga-fifo-cursor*) c)
  (setq *svga-fifo-cursor* (+ *svga-fifo-cursor* (array-size c)))
  (when (>= *svga-fifo-cursor* *svga-fifo-size*)
    (setq *svga-fifo-cursor* *svga-fifo-start*))
  (let ((next (new-stream)))
    (write-dword next *svga-fifo-cursor*)
    (memcpy (+ *svga-fifo-base* 8) (ostream-data next)))
  (svga-sync))

(defun svga-update (x y width height)
  "Обновление области экрана из фреймбуфера"
  (let ((s (new-stream)))
    (dolist (val (list +svga-cmd-update+ x y width height))
      (write-dword s val))
    (svga-command (ostream-data s))))
