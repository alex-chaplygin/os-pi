(defconst +deflate-no-compress+ 0) ;; блок без сжатия
(defconst +deflate-fixed-huff+ 1) ;; блок сжатый с предопределенными кодами Хаффмана
(defconst +deflate-dynamic-huff+ 2) ;; блок сжатый с динамическими кодами Хаффмана

(defun deflate-no-compress ()
  "Блок без сжатия"
  (parse-suc 'NO-COMPRESS))

(defun deflate-fix-huff ()
  "Блок с фиксированными кодами Хаффмана"
  (parse-suc 'FIX-HUFF))

(defun deflate-make-lens (len-ar)
  "Подсчёт массива lens по списку длин кодов"
  (let* ((max-bits (array-max len-ar))
	 (lens (make-array (+ max-bits 1))))      
      (for i 0 (+ max-bits 1)
           (seta lens i 0))
      (for i 0 (array-size len-ar)
           (let ((len (aref len-ar i)))
             (when (> len 0)
               (seta lens len (++ (aref lens len))))))
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
    (huff-make-code-lens (deflate-make-lens s-len) (list-to-array s-vals))))

(defun deflate-mk-ccode-huff (list)
  "Построить дерево Хаффмана для декодирования динамических кодов"
  (deflate-make-code-huff list #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)))

(defun deflate-read-huff ()
  "Чтение представления динамических кодов Хаффмана, построение дерева"
  (&&& hlit->(parse-bits 5) ; длина последовательности для кодов символов/длины 
       hdist->(parse-bits 5) ; длина последовательности для кодов расстояний
       hclen->(parse-bits 4) ; число кодов для алфавита кодирования
       ; дерево Хаффмана для декодирования кодов
       huff->(parse-app (parse-many-n (+ hclen 4) (parse-bits 3)) #'deflate-mk-ccode-huff))
  )

(defun deflate-dynamic-huff ()
  "Блок с динамическими кодами Хаффмана"
  (&&& huff->(deflate-read-huff)))

(defun deflate-block()
  "Чтение блока DEFLATE"
  (&&& #'get-bit (parse-or
		  (&&& (parse-elem-bits 2 +deflate-no-compress+) (deflate-no-compress))
		  (&&& (parse-elem-bits 2 +deflate-fixed-huff+) (deflate-fix-huff))
		  (&&& (parse-elem-bits 2 +deflate-dynamic-huff+) (deflate-dynamic-huff)))))





      

  
