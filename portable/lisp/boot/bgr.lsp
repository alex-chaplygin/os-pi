(defvar +bgr-index+ 0x1ce) ; порт выбора регистра
(defvar +bgr-data+ 0x1cf) ; порт данных
; регистры
(defvar +bgr-width+ 1) ; ширина экрана 
(defvar +bgr-height+ 2) ; высота экрана 
(defvar +bgr-depth+ 3) ; глубина цвета экрана
(defvar +bgr-enable+ 4) ; VBE расширения включены
;(defvar +bgr-bank+ 5) ; порт банка

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
