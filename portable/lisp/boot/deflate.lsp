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
   (deflate-fix-dist 0 0 1)))

(defun deflate-fix-length (huff val extra len)
  "Декодирование кода длины по значению val, дополнительные биты bits, смещение длины len"
  (&&& (parse-elem-huff huff val) n-> (parse-bits extra) return (+ len n)))

(defun deflate-length (huff)
  "Декодирование кода длины по таблице huff"
  (parse-or
   (deflate-fix-length huff 258 0 4)
   (deflate-fix-length huff 265 1 11)
   (deflate-fix-length huff 267 1 15)
   (deflate-fix-length huff 271 2 27)))

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
  
