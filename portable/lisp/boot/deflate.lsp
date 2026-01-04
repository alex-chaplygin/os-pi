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

(defun deflate-dist ()
  "Декодирование кода расстояния по таблице huff"  
  #'(lambda (st)
      (let* ((codes '#((0 1)(7 257)(3 17)(11 4097)(1 5)(9 1025)(5 65)(13 16385)(0 3)(8 513)(4 33)(12 8193)(2 9)
		       (10 2049)(6 129)(nil nil)(0 2)(7 385)(3 25)(11 6145)(1 7)(9 1537)(5 97)(13 24577)(0 4)
		       (8 769)(4 49)(12 12289)(2 13)(10 3073)(6 193)))
	     (r (get-bits st 5))
	     (code (car r))
	     (tab (aref codes code))
	     (bits (get-bits (cdr r) (car tab))))
	(cons (+ (second tab) (car bits)) (cdr bits)))))

(defun deflate-lit-or-length (huff)
  "Декодирование кода символа или пары длины и расстояния по таблице huff"
  #'(lambda (st)
      (let* ((codes '#((0 3)(0 4)(0 5)(0 6)(0 7)(0 8)(0 9)(0 10)(1 11)(1 13)(1 15)(1 17)(2 19)(2 23)(2 27)(2 31)
		       (3 35)(3 43)(3 51)(3 59)(4 67)(4 83)(4 99)(4 115)(5 131)(5 163)(5 195)(5 227)(0 258)))
	     (r (funcall (huff-decode huff) st))
	     (code (if r (car r) nil)))
	(if (= code +deflate-eof+) nil
	    (if (< code 257) (cons code (cdr r))
		(let* ((tab (aref codes (- code 257)))
		       (bits (get-bits (cdr r) (car tab)))
		       (len (+ (second tab) (car bits)))
		       (dist (funcall (deflate-dist) (cdr bits))))
		  (cons (list len (car dist)) (cdr dist))))))))

(defun decode-lz77 (list)
  "Декодирование пар длин и расстояний LZ77"
  (let ((s (new-stream)))
    (labels ((lz77 (l)
	       (if (null l) (ostream-data s)
		   (let ((el (car l))
			 (arr (ostream-arr s)))
		     (cond ((atom el) (write-byte s el))
			   ((pairp el)
			    (for i 0 (car el)
				 (write-byte s (aref arr (- (ostream-ptr s) (second el)))))))
		     (lz77 (cdr l))))))
      (lz77 list))))

(defun deflate-lz77 (huff)
  "Декодирование LZ77 по таблице huff"
  (parse-app (parse-many (deflate-lit-or-length huff)) #'decode-lz77))

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
			     (&&& (parse-elem-bits 2 +deflate-fixed-huff+) (parse-rec (deflate-fix-huff)))
			     (&&& (parse-elem-bits 2 +deflate-dynamic-huff+) (deflate-dynamic-huff))))
       #'cadadr))

(defun deflate (arr)
  "Разпаковать массив arr по алгоритму Deflate"
  (let ((j (funcall (deflate-block) (stream-from-arr arr nil))))
    (if j (car j) (error "Invalid deflate stream"))))
  
