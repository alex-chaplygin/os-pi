(defconst +text-width+ 80) ; число колонок экрана
(defconst +text-height+ 25) ; число строк экрана
(defconst +buffer-size+ (* (* +text-width+ +text-height+) 2)) ; размер буфера экрана
(defvar *text-buffer* (make-array +buffer-size+))

(defun clear-screen()
  "Очистка экрана"
  (for i 0 +buffer-size+
       (seta *text-buffer* i 0))
  (send-text-buffer *text-buffer* 0 0 +text-width+ +text-height+))
