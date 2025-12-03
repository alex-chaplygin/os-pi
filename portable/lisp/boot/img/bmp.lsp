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
      (cons nil stream)))

(defun bmp ()
  (&&& (bmp-signature)           
       header->(parse-bmp-file-header)    
       info->(parse-bmp-info-header)   
       (parse-color-table info) 
       (parse-pixel-data header)))
