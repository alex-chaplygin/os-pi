(defconst +deflate-no-compress+ 0) ;; блок без сжатия
(defconst +deflate-fixed-huff+ 1) ;; блок сжатый с предопределенными кодами Хаффмана
(defconst +deflate-dynamic-huff+ 2) ;; блок сжатый с динамическими кодами Хаффмана

(defun read-block-header (stream)
  "Чтение заголовка блока DEFLATE"
  (let* ((first-bit (get-bits stream 1))
	 (bfinal (car first-bit))
	 (next-bits (get-bits (cdr first-bit) 2))
	 (btype (car next-bits))
	 (new-stream (cdr next-bits)))

    (if (= btype 3)
	nil
      (cons (list bfinal btype) new-stream))))

(defun deflate-no-compress ()
  "Блок без сжатия"
  (parse-suc 'NO-COMPRESS))

(defun deflate-fix-huff ()
  "Блок с фиксированными кодами Хаффмана"
  (parse-suc 'FIX-HUFF))

(defun deflate-make-code-huff (list)
  "Построить дерево Хаффмана по списку длин кодов со значениями
  16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15"
  list)

(defun deflate-read-huff ()
  "Чтение представления кодов Хаффмана, построение дерева"
  (&&& hlit->(parse-bits 5) ; длина последовательности для кодов символов/длины 
       hdist->(parse-bits 5) ; длина последовательности для кодов расстояний
       hclen->(parse-bits 4) ; число кодов для алфавита кодирования
       (parse-app (parse-many-n (+ hclen 4) (parse-bits 3)) #'deflate-make-code-huff)) ; алфавит кодирования кодов
  )

(defun deflate-dynamic-huff ()
  "Блок с фиксированными кодами Хаффмана"
  (&&& huff->(deflate-read-huff)))

(defun deflate-block()
  "Чтение блока DEFLATE"
  (&&& #'get-bit (parse-or
		  (&&& (parse-elem-bits 2 +deflate-no-compress+) (deflate-no-compress))
		  (&&& (parse-elem-bits 2 +deflate-fixed-huff+) (deflate-fix-huff))
		  (&&& (parse-elem-bits 2 +deflate-dynamic-huff+) (deflate-dynamic-huff)))))





      

  
