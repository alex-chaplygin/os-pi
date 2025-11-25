(defconst +BMP-SIGNATURE+ #(0x42 0x4D)) ; "BM"

(defun bmp-signature ()
  "Ожидание сигнатуры BMP"
  (parse-elem-array +BMP-SIGNATURE+))

(defun parse-bmp-file-header ()
  "Читаем остальную часть заголовка файла 12 байт после сигнатуры"
  (parse-struct '((file-size . dword)
                  (reserved1 . word)  ; Зарезервировано 0
                  (reserved2 . word)  ; Зарезервировано 0
                  (offset-data . dword)))) ; Смещение до начала данных изображения

(defun parse-bmp-info-header ()
  "Читаем BITMAPINFOHEADER 40 байт"

  (parse-struct '((header-size . dword) ; Размер этого заголовка 40
                  (width . dword)
                  (height . dword)
                  (planes . word)  ; Число плоскостей всегда 1
                  (bit-count . word)  ; Бит на пиксель 1, 4, 8, 16, 24, 32
                  (compression . dword) ; Тип сжатия 0 = без сжатия
                  (size-image . dword) 
                  (x-pels-per-meter . dword) ; Горизонтальное разрешение
                  (y-pels-per-meter . dword) ; Вертикальное разрешение
                  (colors-used . dword)
                  (colors-important . dword))))

(defun parse-rgb-quad ()
  (parse-struct '((blue . byte)
		  (green . byte)
		  (red . byte)
		  (reserved . byte))))

(defun bmp ()
  "Парсинг всего BMP файла: Сигнатура > Заголовок файла > Инфо-заголовок"
  (&&& (bmp-signature)           
       (parse-bmp-file-header)   
       (parse-bmp-info-header)
       (parse-rgb-quad))) 
