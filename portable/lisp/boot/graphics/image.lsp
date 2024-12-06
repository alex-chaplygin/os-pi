(defun make-image (matrix)
  "Создать изображение с матрицей пикселей. Первое измерение - строки, второе - столбцы."
  (let ((width (array-size (aref matrix 0)))
	(height (array-size matrix)))
    (cons matrix (cons width height))))

(defun image-width (i)
  "Получить ширину изображения"
  (cadr i))

(defun image-height (i)
  "Получить высоту изображения"
  (cddr i))

(defun image-row (i row)
  "Получить строку пикселей с индексом row изображения"
  (aref (car i) row))
