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
   Возвращает точечную пару (символ . новое состояние потока) или nil если конец потока"
  (let ((str (SStream-str self))
	(index (SStream-index self)))
    (if (= index (string-size str)) nil
      (cons (char str index) (make-SStream str (++ index))))))


;; Поток на основе списка
(defclass LStream() (list ; список
		     ))

(defun stream-from-list (list)
  "Создает поток из списка"
  (make-lstream list))

(defmethod get-byte ((self lstream))
  "Чтение очередного элемента списка из потока
   Возвращает точечную пару (элемент . новое состояние потока) или nil, если конец потока"
  (let ((list (lstream-list self)))
    (if (null list) nil
      (cons (car list) (make-lstream (cdr list))))))

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

(defun get-word (self)
  "Чтение очередного слова из потока (16 бит)
   Возвращает точечную пару (число . новое состояние потока) или nil если конец потока"
  (let* ((arr (astream-arr self))
	 (index (astream-byte-num self))
	 (r1 (get-byte self))
	 (r2 (if (null r1) nil (get-byte (cdr r1)))))
    (if (or (null r1) (null r2)) nil
	(if (astream-endianness self) (cons (+ (<< (car r1) 8) (car r2)) (cdr r2))
	  (cons (+ (car r1) (<< (car r2) 8)) (cdr r2))))))

(defun get-dword (self)
  "Чтение очередного слова из потока (32-бит)
   Возвращает точечную пару (число . новое состояние потока) или nil если конец потока"
  (let* ((arr (astream-arr self))
	 (index (astream-byte-num self))
	 (r1 (get-word self))
	 (r2 (if (null r1) nil (get-word (cdr r1)))))
    (if (or (null r1) (null r2)) nil
	(if (astream-endianness self) (cons (+ (<< (car r1) 16) (car r2)) (cdr r2))
	  (cons (+ (car r1) (<< (car r2) 16)) (cdr r2))))))

(defun get-4bit (self)
  "Чтение пары по 4 бита из потока
   Возвращает точечную пару (пара по 4 бита . новое состояние потока) или nil если конец потока"
  (let ((arr (astream-arr self))
	(index (astream-byte-num self))
	(r1 (get-byte self)))
    (if (null r1) nil
	(cons (cons (>> (car r1) 4) (& (car r1) 0xf)) (cdr r1)))))

(defun get-array (self num)
  "Чтение массива (n-байт) из потока
   Возвращает точечную пару (массив . новое состояние потока) или nil если конец потока"
  (let ((res (make-array num)))
    (for i 0 num
	 (let ((b (get-byte self)))
	   (seta res i (car b))
	   (setq self (cdr b))))
    (cons res self)))
	
(defun get-bit (self)
  "Читает один бит из потока"
  (let ((arr (astream-arr self))
	(bit-num (astream-bit-num self))
	(end (astream-endianness self))
        (byte-num (astream-byte-num self)))
    (when (= bit-num (if end 0 7))
      (setq byte-num (++ byte-num))
      (setq bit-num (if end 8 -1)))
    (let ((new-bit-num (if end (-- bit-num) (++ bit-num))))
      (cons (& 1 (>> (aref arr byte-num) new-bit-num))
	    (make-astream arr byte-num new-bit-num end)))))

(defun get-struct (stream struct)
  "Читает из потока структуру по шаблону"
  "Шаблон: ((accuracy . byte) (height . word) (width . word) (other . bits4) (array . 16))"
  "Возвращает хэш-таблицу"
  (labels ((read (st type)
		 (case type
		       ('byte (get-byte st))
		       ('word (get-word st))
		       ('bits4 (get-4bit st))
		       (otherwise (get-array st type)))))
	  (let ((table (make-hash)))
	    (cons table (foldl #'(lambda (st x)
				   (let ((res (read st (cdr x))))
				     (set-hash table (car x) (car res))
				     (cdr res))) stream struct)))))
