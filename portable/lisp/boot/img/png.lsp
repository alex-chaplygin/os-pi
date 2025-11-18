(defconst +PNG-SIGNATURE+ #(0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a)) ;Сигнатура PNG изображения

(defun make-png-sig (str)
  "Преобразование сигнатуры PNG в массив байт"
  (list-to-array (map #'char-code (explode str))))

(defconst +IHDR+ (make-png-sig "IHDR")) ;Заголовок файла
;;(defconst +PLTE+ "PLTE") ;Палитра, список цветов
;;(defconst +IDAT+ "IDAT") ;Изображение
(defconst +IEND+ "IEND") ;Завершающий чанк

(print +IHDR+)

(defun png-signature ()
  "Ожидание сигнатуры PNG"
  (parse-elem-array +PNG-SIGNATURE+))

(defun parse-ihdr ()
  "Читаем IHDR блок изображения PNG"
  (parse-app (&&& #'get-dword
	      (parse-elem-array +IHDR+)
	      (parse-struct '((width . dword) (height . dword)
			      (bit-depth . byte) (color-type . byte)
			      (compression . byte) (filter . byte) (interlace . byte))))
	     #'third))


;;(defun parse-idat ()

(defun png ()
  (&&& (png-signature) (parse-ihdr)))

;; (defun parse-chunk ()
;;   "Чтение чанка PNG"
;;   #'(lambda (stream)
;;       (let* ((
