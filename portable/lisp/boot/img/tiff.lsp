;; Хэш-таблица типов
(defvar *type* 
  '(hash
    (1 . BYTE)
    (2 . ASCII)
    (3 . SHORT)
    (4 . LONG)
    (5 . RATIONAL)
    (6 . SBYTE)
    (7 . UNDEFINED)
    (8 . SSHORT)
    (9 . SLONG)
    (10 . SRATIONAL)
    (11 . FLOAT)
    (12 . DOUBLE)))

;; Хэш-таблица тегов
(defvar *tag* 
  '(hash
    (256 . ImageWidth)
    (257 . ImageLength)
    (258 . BitsPerSample)
    (259 . Compression)
    (262 . PhotometricInterpretation)
    (266 . FillOrder)
    (273 . StripOffsets)
    (274 . Orientation)
    (277 . SamplesPerPixel)
    (278 . RowsPerStrip)
    (279 . StripByteCounts)
    (282 . XResolution)
    (283 . YResolution)
    (284 . PlanarConfiguration)
    (292 . T4Options)
    (296 . ResolutionUnit)
    (297 . PageNumber)
    (320 . ColorMap)))

;; Конвертирует числовой тип в символьное представление
(defun get-type (type-number)
  (get-hash *type* type-number))

;; Конвертирует числовой тег в символьное представление
(defun get-tag (tag-number)
  (get-hash *tag* tag-number))

(defun tiff-header ()
  (&&& 
   #'(lambda (stream)
       (let* ((res (get-word stream))
	      (st (cdr res)))
	 (cons (car res) (make-astream (astream-arr st) (astream-byte-num st) (astream-bit-num st)
				       (if (= 0x4949 (car res)) nil t)))))
   (parse-elem-word 42)
   #'(lambda (stream)
       (let* ((res (get-dword stream))
	      (st (cdr res)))
	 (cons (car res) (stream-seek st (car res) 'seek-set))))))

(defun tiff-directory ()
  (&&&
   #'(lambda (stream)
       (let* ((st (get-word stream))
	      (num (car st))
	      (res (make-array num)))
	 (setq st (cdr st))
	 (for i 0 num
	      (let ((b (get-struct st '((tag . word) (type . word) (count . dword) (offset . dword)))))
		(let ((c (car b)))
		  (set-hash c 'tag (get-tag (get-hash c 'tag)))
		  (set-hash c 'type (get-type (get-hash c 'type)))
		  (seta res i c)
		  (setq st (cdr b)))))
	 (cons res stream)))))

(defun tiff ()
  (&&&
   (tiff-header)
   (tiff-directory)))
