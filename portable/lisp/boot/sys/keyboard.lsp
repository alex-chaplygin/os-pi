(defconst +KEY-STATUS+ 0x64) ; номер порта статуса клавиатуры
(defconst +KEY-BUFFER+ 0x60) ; номер порта буфера клавиатуры

(defconst +key-irq+ 1) ; номер линии прерывания клавиатуры
(defconst +key-left+ 0x4b)
(defconst +key-right+ 0x4d)
(defconst +key-up+ 0x48)
(defconst +key-down+ 0x50)
(defconst +key-0+ 0x0B)
(defconst +key-1+ 0x02)
(defconst +key-2+ 0x03)
(defconst +key-3+ 0x04)
(defconst +key-4+ 0x05)
(defconst +key-5+ 0x06)
(defconst +key-6+ 0x07)
(defconst +key-7+ 0x08)
(defconst +key-8+ 0x09)
(defconst +key-9+ 0x0A)

(defvar *keys* (make-array 128)) ; массив нажатий клавиш

(defun key-handler ()
  "Обработчик прерывания клавиатуры"
  (setq *status* (inb +KEY-STATUS+)) ; получает статус, есть ли данные в буфере клавиатуры
  (if (equal (& *status* 1) 1) ; если есть (младший бит регистра статуса)
      (progn
	(setq *scan* (inb +KEY-BUFFER+)) ; читаем скан код из буфера
	(if (< *scan* 128)
	    (progn (seta *keys* *scan* t) ; если меньше 128, то это нажатие клавиши
		   (queue-push *events* (cons 'keydown *scan*)))
	    (progn
	      (seta *keys* (- *scan* 128) nil) ; иначе это отпускание клавиши
	      (queue-push *events* (cons 'keyup (- *scan* 128)))))) nil))

(defun key-pressed (key)
  "Вовращает состояние нажатия клавиши key"
  (aref *keys* key))

;(set-int-handler +key-irq+ 'key-handler)
