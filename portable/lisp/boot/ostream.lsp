;; Размер буфера
(defconst +ostream-size+ 1024)

;; Класс OStream
;; arr - массив байт
;; ptr - текущая позиция записи (индекс)
(defclass OStream () (arr ptr))

(defun new-stream ()
  "Создает новый поток записи"
    (make-OStream (alloc +ostream-size+) 0))

(defmethod write-byte ((self OStream) byte)
  "Записать 1 байт в поток и сдвинуть курсор"
  (let ((arr (OStream-arr self))
        (ptr (OStream-ptr self)))
    (seta arr ptr byte)
    (OStream-set-ptr self (+ ptr 1))))

(defmethod write-word ((self OStream) w)
  "Записать слово (2 байта, Little Endian)"
  ;; Пишем младший байт
  (write-byte self w)
  ;; Пишем старший байт
  (write-byte self (>> w 8)))

(defmethod write-dword ((self OStream) dw)
  "Записать двойное слово (4 байта) как 2 слова (Little Endian)"
  (write-word self dw)
  (write-word self (>> dw 16)))

(defun ostream-data (s)
  "Возвращает только записанную часть потока как массив байт"
  (let* ((size (OStream-ptr s))
	 (a (alloc size))
	 (ar (Ostream-arr s)))
    (for i 0 size (seta a i (aref ar i)))
    a))
