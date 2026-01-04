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
  (&&& len-> #'get-dword ;Длина
       (parse-elem-array type) ;Тип
       data-> (funcall data-parser len);Данные
       #'get-dword ;CRC
       return data))

(defun parse-ihdr (len)
  "Заголовок изображения PNG"
  (parse-struct '((width . dword) (height . dword)
		  (bit-depth . byte) (color-type . byte)
		  (compression . byte) (filter . byte) (interlace . byte))))

(defun get-components (color-type)
  "Количество компонентов на пиксель"
    (case color-type (0 1) (2 3) (3 1) (4 2) (6 4) (t (error "Invalid color type"))))

(defun parse-plte (len)
  "Читаем палитру (RGB)"
  (parse-suc 'PLTE))

(defun parse-srgb (len)
  "Пространство цвета RGB"
  (parse-array len))

(defun zlib-decode (arr)
  "Распаковать массив zlib"
  (let* ((s (stream-from-arr arr nil))
	 (s2 (funcall (&&& cm->(parse-bits 4)
			  cinfo->(parse-bits 4)
			  fcheck->(parse-bits 5)
			  fdict-> #'get-bit
			  flevel->(parse-bits 2)) s)))
    (deflate (array-seq arr 2 (- (array-size arr) 4)))))

(defun parse-idat (len)
  "Данные изображения"
  (&&& data-> (parse-array len)
       return  (zlib-decode data)))

(defun filter-row (row comp)
  "Фильтр Sub"
  (let ((filter (aref row 0))
	(out (make-array (-- (array-size row))))
	(pos 0)
	(c 0)
	(prev (make-array comp)))
    (for i 0 comp (seta prev i 0))
    (for x 1 (array-size row)
	 (let ((new (case filter
		      (0 (aref row x))
		      (1 (& (+ (aref row x) (aref prev c)) 0xff))
		      (otherwise (error "PNG: unknown filter" filter)))))
	   (seta out pos new)
	   (seta prev c new)
	   (incf pos)
	   (incf c)
	   (when (= c comp) (setq c 0))))
    out))

(defun array-to-3array (arr width height comp)
  "Преобразовать распакованный массив в матрицу пикселей"
  (let ((matrix (make-array height))
	(idx 0))
    (for y 0 height
	 (let* ((row (array-seq arr idx (+ idx (* comp width) 1)))
		(frow (filter-row row comp))
		(newrow (make-array width)))
	   (incf idx)
	   (let ((pos 0))
	     (for x 0 width
		  (let ((pixel (make-array comp)))
		    (for i 0 comp
			 (seta pixel i (aref frow pos))
			 (incf idx)
			 (incf pos))
		    (seta newrow x pixel))))
	   (seta matrix y newrow)))
    matrix))

(defun png ()
  "Разбор PNG"
  (&&& (png-signature)
       header-> (parse-chunk +IHDR+ #'parse-ihdr)
       (parse-optional (parse-or (parse-chunk +PLTE+ #'parse-plte)
       				 (parse-chunk +sRGB+ #'parse-srgb)))
       data-> (parse-chunk +IDAT+ #'parse-idat)
       (parse-chunk +IEND+ #'(lambda (x) (parse-suc nil)))
       return (array-to-3array data (get-hash header 'width) (get-hash header 'height)
       			       (get-components (get-hash header 'color-type)))))

(defun decode-png (png)
  "Декодирование PNG"
  (let ((j (funcall (png) (stream-from-arr png t))))
    (if j (car j) j)))
