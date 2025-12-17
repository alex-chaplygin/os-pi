(defconst +BMP-SIGNATURE+ #(0x42 0x4D))

(defun get-val (alist key);достать значение из списка по ключу
  (cdr (assoc key alist)))

(defun parse-save (parser var-symbol);выполняет парсер и сохраняет результат в глобальную переменную"
  (lambda (stream)
    (let ((result (funcall parser stream)))
      (set var-symbol result)
      result)))

(defun bmp-signature ()
  (parse-elem-array +BMP-SIGNATURE+))

(defun parse-bmp-file-header ()
  (parse-struct '((file-size . dword)
		  (reserved1 . word)
		  (reserved2 . word)
		  (offset-data . dword))))

(defun parse-bmp-info-header ()
  (parse-struct '((header-size . dword)
		  (width . dword)
		  (height . dword)
		  (planes . word)
		  (bit-count . word)
		  (compression . dword)
		  (size-image . dword)
		  (x-pels . dword)
		  (y-pels . dword)
		  (colors-used . dword)
		  (colors-imp . dword))))

(defun parse-rgb-quad ()
  (parse-struct '((blue . byte)
                  (green . byte)
                  (red . byte)
                  (reserved . byte))))

(defun parse-color-table (info);таблица цветов
  (parse-many-n (let ((bits (get-hash info 'bit-count))
		      (used (get-hash info 'colors-used)))
		  (cond ((<= bits 8) 0)
			((= used 0) (<< 1 bits))
			(t used))) (parse-rgb-quad)))

(defun parse-pixel-data (header);перемотка на offset-data и чтение массив size-image"
  #'(lambda (stream)
      (let* ((offset (get-hash header 'offset-data))
	     (size (get-hash info 'size-image))
	     (pixel-array (get-array (stream-seek stream offset 'seek-set) size)))
      pixel-array)))

;матрица
(defun parse-matrix-array (info pixel-array)
  (let* ((width (get-hash info 'width))
         (height (get-hash info 'height))
         (bytes-per-pixel 4)
         (row-data-len (* width bytes-per-pixel))
         (stride (* (+ row-data-len 3) 4))
         (matrix '())) 

    ;цикл строк
    (for i 0 height
      (let* ((offset (* i stride)) ;смещение
             (row '()))            
        
        (for p 0 width
          (let* ((k (* p bytes-per-pixel));смещение
                 (blue (aref pixel-array (+ offset k)))
                 (green (aref pixel-array (+ offset k 1)))
                 (red (aref pixel-array (+ offset k 2)))
                 (alpha (aref pixel-array (+ offset k 3)))
                 
                 (pixel (list blue green red alpha)))
            
            (setq row (cons pixel row))))

        (setq matrix (cons (reverse row) matrix))))
    
    matrix))

(defun bmp ()
  (&&& (bmp-signature)           
       header->(parse-bmp-file-header)    
       info->(parse-bmp-info-header)   
       (parse-color-table info) 
       pixels->(parse-pixel-data header)
       return (parse-matrix-array info pixels)))
