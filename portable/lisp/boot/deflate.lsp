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

(defun deflate-block-header ()
  "Чтение заголовка блока DEFLATE"
  (&&& #'get-bit (parse-bits 2)))
