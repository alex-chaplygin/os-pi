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
  "Заголовок TIFF"
  (&&& end-> #'get-word
   #'(lambda (st)
   	 (cons end (make-astream (astream-arr st) (astream-byte-num st) (astream-bit-num st)
   				       (if (= 0x4949 end) nil t))))
   (parse-elem-word 42)
   ofs-> #'get-dword
   #'(lambda (st) (cons ofs (stream-seek st ofs 'seek-set)))))

(defun tiff-directory ()
  "Каталог полей TIFF"
  (&&& num-> #'get-word
       hash->(parse-many-n num (parse-struct '((tag . word) (type . word) (count . dword) (offset . dword))))
       return (map #'(lambda (v) (cons (get-tag (get-hash v 'tag)) (get-hash v 'offset))) hash)))

(defun tiff ()
  (parse-app
   (&&&
    (tiff-header)
    (tiff-directory))
     #'second))

(defun decode-tiff (arr)
  (let* ((stream (stream-from-arr arr t))
	 (tiff-hash (car (funcall (tiff) stream)))
	 (a (print `(tiff ,tiff-hash)))
	 (ofs (get-hash tiff-hash 'stripoffsets))
	 (count (get-hash tiff-hash 'stripbytecounts)))
    (print (car (get-array (stream-seek stream ofs 'seek-set) count)))))
