(defconst +deflate-no-compress+ 0) ;; блок без сжатия
(defconst +deflate-fixed-huff+ 1) ;; блок сжатый с предопределенными кодами Хаффмана
(defconst +deflate-dynamic-huff+ 2) ;; блок сжатый с динамическими кодами Хаффмана
(defconst +deflate-eof+ 256) ;; код окончания потока

(defvar *fixed-huffman* (make-huff))
(for i 0 144 (setq *fixed-huffman* (huff-add *fixed-huffman* (+ i 0x30) 8 i)))
(for i 144 256 (setq *fixed-huffman* (huff-add *fixed-huffman* (+ 0x190 (- i 144)) 9 i)))
(for i 256 280 (setq *fixed-huffman* (huff-add *fixed-huffman* (- i 256) 7 i)))
(for i 280 288 (setq *fixed-huffman* (huff-add *fixed-huffman* (+ 0xC0 (- i 280)) 8 i)))

(defun deflate-no-compress ()
  "Блок без сжатия"
  (parse-suc 'NO-COMPRESS))

(defun deflate-fix-dist (val extra dist)
  "Декодирование расстояния по значению val, дополнительные биты extra, смещение расстояния dist"
  (&&& (parse-elem-bits 5 val) n-> (parse-bits extra) return (+ dist n)))

(defun deflate-dist ()
  "Декодирование кода расстояния по таблице huff"  
  (parse-or
   (deflate-fix-dist 0 0 1)
   (deflate-fix-dist 1 0 2)
   (deflate-fix-dist 2 0 3)
   (deflate-fix-dist 3 0 4)
   (deflate-fix-dist 4 1 5)
   (deflate-fix-dist 5 1 7)
   (deflate-fix-dist 6 2 9)
   (deflate-fix-dist 7 2 13)
   (deflate-fix-dist 8 3 17)
   (deflate-fix-dist 9 3 25)
   (deflate-fix-dist 10 4 33)
   (deflate-fix-dist 11 4 49)
   (deflate-fix-dist 12 5 65)
   (deflate-fix-dist 13 5 97)
   (deflate-fix-dist 14 6 129)
   (deflate-fix-dist 15 6 193)
   (deflate-fix-dist 16 7 257)
   (deflate-fix-dist 17 7 385)
   (deflate-fix-dist 18 8 513)
   (deflate-fix-dist 19 8 769)
   (deflate-fix-dist 20 9 1025)
   (deflate-fix-dist 21 9 1537)
   (deflate-fix-dist 22 10 2049)
   (deflate-fix-dist 23 10 3073)
   (deflate-fix-dist 24 11 4097)
   (deflate-fix-dist 25 11 6145)
   (deflate-fix-dist 26 12 8193)
   (deflate-fix-dist 27 12 12289)
   (deflate-fix-dist 28 13 16385)
   (deflate-fix-dist 29 13 24577)))

(defun deflate-fix-length (huff val extra len)
  "Декодирование кода длины по значению val, дополнительные биты bits, смещение длины len"
  (&&& (parse-elem-huff huff val) n-> (parse-bits extra) return (+ len n)))

(defun deflate-length (huff)
  "Декодирование кода длины по таблице huff"
  (parse-or
   (deflate-fix-length huff 257 0 3)
   (deflate-fix-length huff 258 0 4)
   (deflate-fix-length huff 259 0 5)
   (deflate-fix-length huff 260 0 6)
   (deflate-fix-length huff 261 0 7)
   (deflate-fix-length huff 262 0 8)
   (deflate-fix-length huff 263 0 9)
   (deflate-fix-length huff 264 0 10)
   (deflate-fix-length huff 265 1 11)
   (deflate-fix-length huff 266 1 13)
   (deflate-fix-length huff 267 1 15)
   (deflate-fix-length huff 268 1 17)
   (deflate-fix-length huff 269 2 19)
   (deflate-fix-length huff 270 2 23)
   (deflate-fix-length huff 271 2 27)
   (deflate-fix-length huff 272 2 31)
   (deflate-fix-length huff 273 3 35)
   (deflate-fix-length huff 274 3 43)
   (deflate-fix-length huff 275 3 51)
   (deflate-fix-length huff 276 3 59)
   (deflate-fix-length huff 277 4 67)
   (deflate-fix-length huff 278 4 83)
   (deflate-fix-length huff 279 4 99)
   (deflate-fix-length huff 280 4 115)
   (deflate-fix-length huff 281 5 131)
   (deflate-fix-length huff 282 5 163)
   (deflate-fix-length huff 283 5 195)
   (deflate-fix-length huff 284 5 227)
   (deflate-fix-length huff 285 0 258)))

(defun decode-lz77 (list)
  "Декодирование пар длин и расстояний LZ77"
  ;;(print `(lz77 ,list))
  (let ((s (new-stream)))
    (labels ((lz77 (l)
	       (if (null l) (error "Invalid LZ77 stream")
		   (let ((el (car l))
			 (arr (ostream-arr s)))
		     (if (= el +deflate-eof+) (ostream-data s)
			 (progn
			   (cond ((atom el) (write-byte s el))
				 ((pairp el)
				  (for i 0 (car el)
				       (write-byte s (aref arr (- (ostream-ptr s) (second el)))))))
			   (lz77 (cdr l))))))))
      (lz77 list))))

(defun deflate-lz77 (huff)
  "Декодирование LZ77 по таблице huff"
  (parse-app (parse-many (parse-or (&&& (deflate-length huff) (deflate-dist))
				   (huff-decode *fixed-huffman*))) #'decode-lz77))

(defun deflate-fix-huff ()
  "Блок с фиксированными кодами Хаффмана"
  (deflate-lz77 *fixed-huffman*))

(defun deflate-make-lens (len-ar)
  "Подсчёт массива lens по списку длин кодов"
  (let* ((max-bits (array-max len-ar))
	 (lens (make-array max-bits)))
      (for i 0 max-bits
           (seta lens i 0))
      (for i 0 (array-size len-ar)
           (let ((len (aref len-ar i)))
             (when (> len 0)
               (seta lens (-- len) (++ (aref lens (-- len)))))))
    lens))

(defun deflate-make-code-huff (list v)
  "Построить дерево Хаффмана по списку длин кодов list со значениями v"
  (let* ((len (list-length list))
	 (vals (array-to-list (array-seq v 0 len)))
	 (s-len (list-to-array (sort #'< list)))
	 (s-vals (sort #'(lambda (a b)
			   (let ((i (list-search vals a))
				 (j (list-search vals b)))
			     (< (nth list i) (nth list j)))) vals)))
    (print `(list ,list s-len ,s-len s-vals ,s-vals ,(deflate-make-lens s-len)))
    (huff-make-code-lens (deflate-make-lens s-len) (list-to-array s-vals))))

(defun deflate-mk-ccode-huff (list)
  "Построить дерево Хаффмана для декодирования динамических кодов"
  (deflate-make-code-huff list #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)))

(defun deflate-decode-lens (huff size)
  "Декодировать длины кодов числом size, используя дерево huff"
  (parse-many-n size (huff-decode huff)))

(defun deflate-read-huff ()
  "Чтение представления динамических кодов Хаффмана, построение дерева"
  (&&& hlit->(parse-bits 5) ; длина последовательности для кодов символов/длины 
       hdist->(parse-bits 5) ; длина последовательности для кодов расстояний
       hclen->(parse-bits 4) ; число кодов для алфавита кодирования
       ; дерево Хаффмана для декодирования кодов
       huff->(parse-app (parse-many-n (+ hclen 4) (parse-bits 3)) #'deflate-mk-ccode-huff)
       (deflate-decode-lens huff 1));;(+ 257 hlit)))
  )

(defun deflate-dynamic-huff ()
  "Блок с динамическими кодами Хаффмана"
  (&&& huff->(deflate-read-huff)))

(defun deflate-block()
  "Чтение блока DEFLATE"
  (parse-app (&&& #'get-bit (parse-or
			     (&&& (parse-elem-bits 2 +deflate-no-compress+) (deflate-no-compress))
			     (&&& (parse-elem-bits 2 +deflate-fixed-huff+) (deflate-fix-huff))
			     (&&& (parse-elem-bits 2 +deflate-dynamic-huff+) (deflate-dynamic-huff))))
       #'cadadr))

(defun deflate (arr)
  "Разпаковать массив arr по алгоритму Deflate"
  (let ((j (funcall (deflate-block) (stream-from-arr arr nil))))
    (if j (car j) (error "Invalid deflate stream"))))
  
