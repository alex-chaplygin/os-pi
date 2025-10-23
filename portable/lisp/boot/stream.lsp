;; Поток на основе строки
(defclass SStream ()
  (str ; строка
   index ; индекс текущего символа
   ))

(defun stream-from-str (str)
  "Создает поток из строки str"
  (make-SStream str 0))

(defmethod get-byte ((self SStream))
  "Чтение очередного символа из потока
   Возвращает точчную пару (символ . новое состояние потока) или nil если конец потока"
  (let ((str (SStream-str self))
	(index (SStream-index self)))
    (if (= index (string-size str)) nil
      (cons (char str index) (make-SStream str (++ index))))))

;; Поток на основе массива
(defclass AStream () (arr ; массив
		      byte-num ; номер текущего байта
		      bit-num ; номер текущего бита
		      endianness ; порядок байт и бит: t - big endian, nil - little endian
		      ))

(defun stream-from-arr (arr end)
  "Создает поток из массива arr с порядком байт end"
  (make-astream arr -1 (if end 0 7) end))

(defmethod get-byte ((self astream))
  "Чтение очередного байта из потока
   Возвращает точечную пару (число . новое состояние потока) или nil если конец потока"
  (let ((arr (astream-arr self))
	(index (++ (astream-byte-num self))))
    (if (= index (array-size arr)) nil
	(cons (aref arr index) (make-astream arr index (astream-bit-num self) (astream-endianness self))))))

(defmethod get-word ((self astream))
  "Чтение очередного слова из потока
   Возвращает точечную пару (число . новое состояние потока) или nil если конец потока"
  (let* ((end (astream-endianness self))
	 (arr (astream-arr self))
	 (index (astream-byte-num self))
	 (r1 (get-byte self))
	 (r2 (if (null r1) nil (get-byte (cdr r1)))))
    (if (or (null r1) (null r2)) nil
	(if end (cons (+ (<< (car r1) 8) (car r2)) (cdr r2))
	    (cons (+ (car r1) (<< (car r2) 8)) (cdr r2))))))
