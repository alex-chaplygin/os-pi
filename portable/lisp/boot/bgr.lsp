(defvar +bgr-index+ 0x1ce) ; порт выбора регистра
(defvar +bgr-data+ 0x1cf) ; порт данных
; регистры
(defvar +bgr-width+ 1) ; ширина экрана 
(defvar +bgr-height+ 2) ; высота экрана 
(defvar +bgr-depth+ 3) ; глубина цвета экрана
(defvar +bgr-enable+ 4) ; VBE расширения включены
(defvar +bgr-bank+ 5) ; порт банка
(defvar +bgr-bank-address+ 0xa0000) ; адрес банка

(defun bgr-in (reg)
  "Чтение регистра видео"
  (outw +bgr-index+ reg)
  (inw +bgr-data+))

(defun bgr-out (reg data)
  "Запись регистра видео"
  (outw +bgr-index+ reg)
  (outw +bgr-data+ data))
  
(defun bgr-set-res (width height depth)
  "Установка разрешения экрана"
  (bgr-out +bgr-enable+ 0)
  (bgr-out +bgr-width+ width)
  (bgr-out +bgr-height+ height)
  (bgr-out +bgr-depth+ depth)
  (bgr-out +bgr-enable+ 1))

(defun bgr-write-pixel (x y colour)
  "Установка цвета пикселя"
  (defvar bpp (bgr-in +bgr-depth+))
  (defvar byp (/ bpp 8))
  (defvar height (* y (bgr-in +bgr-width+)))
  ; (defvar pixaddr (* (+ x height) byp))
  ; (defvar pixbank (/ PIXADDR 65536))
  ; (bgr-out +bgr-bank+ pixbank)
  ; (bgr-out (+ +bgr-bank-address+ pixaddr) colour)
)

(defun bgr-test (a)
  "Проверка видеоадаптера"
  (bgr-set-res 640 480 8)
  ; (bgr-write-pixel 5 5 5)
  ; (defun bgr-set-next-pixel (pixel colour))
)