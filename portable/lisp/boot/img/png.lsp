(defconst +PNG-SIGNATURE+ #(0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a)) ;Сигнатура PNG изображения

(defun make-png-sig (str)
  "Преобразование сигнатуры PNG в массив байт"
  (list-to-array (map #'char-code (explode str))))

(defconst +IHDR+ (make-png-sig "IHDR")) ;Заголовок файла
(defconst +PLTE+ (make-png-sig "PLTE")) ;Палитра, список цветов
(defconst +IDAT+ (make-png-sig "IDAT")) ;Изображение
(defconst +IEND+ (make-png-sig "IEND")) ;Завершающий чанк
(defconst +sRGB+ (make-png-sig "sRGB")) ;Пространство цветов RGB

(defun png-signature ()
  "Ожидание сигнатуры PNG"
  (parse-elem-array +PNG-SIGNATURE+))

(defun parse-chunk (type data-parser)
  "Разбор блоков PNG"
  (parse-app
   (&&& #'get-dword ;Длина
	(parse-elem-array type) ;Тип
	data-parser ;Данные
	#'get-dword) ;CRC
   #'third))

(defun parse-ihdr ()
  "Заголовок изображения PNG"
  (parse-struct '((width . dword) (height . dword)
		  (bit-depth . byte) (color-type . byte)
		  (compression . byte) (filter . byte) (interlace . byte))))

(defun parse-plte ()
  "Читаем палитру (RGB)"
  (parse-suc 'PLTE))

(defun zlib-decode (arr)
  "Распаковать массив zlib"
  (let ((a (array-seq arr 2 (array-size arr))))
    (car (funcall (deflate-block-header) (stream-from-arr a nil)))))

(defun parse-idat ()
  "Данные изображения"
  #'(lambda (stream)
      (let* ((len (get-dword stream))
	     (length (car len))
	     (stream2 (funcall (parse-elem-array +IDAT+) (cdr len))))
	(if (null stream2) nil
	    (let*
	     ((stream3 (get-array (cdr stream2) length))
	     (CRC (get-dword (cdr stream3))))
	     (print 'deflate (zlib-decode (car stream3)))
	     (cons (car stream3) (cdr CRC)))))))

  ;(parse-suc 'IDAT))

(defun png ()
  "Разбор PNG"
  (&&& (png-signature)
       (parse-many (parse-or (parse-chunk +IHDR+ (parse-ihdr))
			     (parse-chunk +PLTE+ (parse-plte))
			     (parse-chunk +sRGB+ #'get-byte)
			     (parse-idat)))
       (parse-chunk +IEND+ (parse-suc nil))))
