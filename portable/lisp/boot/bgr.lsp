(defvar +bgr-index+ 0x1ce) ; порт выбора регистра
(defvar +bgr-data+ 0x1cf) ; порт данных

; чтение регистра видео
(defun bgr-in (reg)
  (outw +bgr-index+ reg)
  (inw +bgr-data+))

; запись регистра видео
(defun bgr-out (reg data)
  (outw +bgr-index+ reg)
  (outw +bgr-data+ data))
  
