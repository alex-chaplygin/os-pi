; Цвета
(defconst +black+ 0)
(defconst +blue+ 1)
(defconst +green+ 2)
(defconst +cyan+ 3)
(defconst +red+ 4)
(defconst +magenta+ 5)
(defconst +brown+ 6)
(defconst +light-gray+ 7)
(defconst +dark-gray+ 8)
(defconst +light-blue+ 9)
(defconst +light-green+ 10)
(defconst +light-cyan+ 11)
(defconst +light-red+ 12)
(defconst +light-magenta+ 13)
(defconst +yellow+ 14)
(defconst +white+ 15)
(defconst +text-width+ 80) ; число колонок экрана
(defconst +text-height+ 25) ; число строк экрана
(defconst +buffer-size+ (* (* +text-width+ +text-height+) 2)) ; размер буфера экрана
(defvar *text-buffer* (make-array +buffer-size+)) ; буфер для текстовой видеопамяти

(defun clear-screen()
  "Очистка экрана"
  (for i 0 +buffer-size+
       (seta *text-buffer* i 0))
  (send-text-buffer *text-buffer* 0 0 +text-width+ +text-height+))

(defun fill-rect (x y w h)
  "Заполняет область пробелом с учетом матрицы преобразования"
  (let* ((c (get-hash *cur-state* 'color))
         (x2 (+ x w))
         (y2 (+ y h))
         (top-left (mat-mul-vec ctm (cons x y)))
         (bottom-right (mat-mul-vec ctm (cons x2 y2)))
         (tx1 (car top-left))
         (ty1 (cdr top-left))
         (tx2 (car bottom-right))
         (ty2 (cdr bottom-right))
         (new-w (- tx2 tx1))
         (new-h (- ty2 ty1)))
    (set-color c)
    (set-back-color c)
    (for i ty1 (+ ty1 new-h)
      (set-cursor tx1 i)
      (fill-row new-w #\ ))))

(defun show-text (text)
  "Вывод текста в текущей позиции"
  (let* ((ctm (get-hash *cur-state* 'ctm))
	 (p (mat-mul-vec ctm *cur-point*))
	 (c (get-hash *cur-state* 'color)))
    (set-cursor (car p) (cdr p))
    (set-color c)
    (putstring text)))
