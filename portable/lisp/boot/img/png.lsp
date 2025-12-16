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
  (&&& #'get-dword ;Длина
       (parse-elem-array type) ;Тип
       data-> data-parser ;Данные
       #'get-dword ;CRC
       return data))

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
    (car (funcall (deflate-block) (stream-from-arr a nil)))))

(defun parse-idat ()
  "Данные изображения"
  (&&& len-> #'get-dword
       (parse-elem-array +IDAT+)
       data-> (parse-array len)
       #'get-dword
       return (zlib-decode data)))

(defun decompressed (arr width height comp)
  "Преобразовать распакованный массив в матрицу пикселей"
  (let ((matrix (make-array height)))
    (for y 0 height
	 (let ((row (make-array width)))
	   (for x 0 width
		(let ((pixel (make-array comp))
		      (idx (* (+ (* y width) x) comp)))
		  (for i 0 comp
		       (seta pixel i (aref arr (+ idx i))))
		  (seta row x pixel)))
	   (seta matrix y row)))
    matrix))

(defun filter-sub (arr width height comp)
  "Фильтр Sub"
  (let ((matrix (make-array height)))
    (for y 0 height
	 (let ((row (make-array width))
	       (prev-pixel (make-array comp)))
	   (for x 0 width
		(let ((pixel (make-array comp))
		      (idx (* (+ (* y width) x) comp)))
		  (for i 0 comp
		       (let ((filtered (aref arr (+ idx i)))
			     (left (if (> x 0)
				       (aref prev-pixel i)
				     0)))
			 (seta pixel i (% (+ filtered left) 256))
			 (seta prev-pixel i (aref pixel i))))
		  (seta row x pixel)))
	   (seta matrix y row)))
    matrix))
  
(defun png ()
  "Разбор PNG"
  (&&& (png-signature)
       (parse-many (parse-or (parse-chunk +IHDR+ (parse-ihdr))
			     (parse-chunk +PLTE+ (parse-plte))
			     (parse-chunk +sRGB+ #'get-byte)
			     (parse-idat)))
       (parse-chunk +IEND+ (parse-suc nil))))
