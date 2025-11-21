;; Абстрактный тип Позиция
(defun init-pos ()
  "Начальная позиция в потоке"
  (list 1 1))

(defun pos-line (pos)
  "Возвращает строку из позиции"
  (car pos))

(defun pos-char (pos)
  "Возвращает колонку (character position) из позиции"
  (second pos))

(defun next-char (pos)
  "Возвращает новую позицию после обычного символа"
  (list (pos-line pos) (++ (pos-char pos))))

(defun next-line (pos)
  "Возвращает новую позицию после перевода строки"
  (list (++ (pos-line pos)) (pos-char pos)))

;; Поток на основе строки с позицией
(defclass PosStream SStream (pos))

(defun stream-from-str (str)
  "Создаёт PosStream из строки"
  (make-PosStream str 0 (init-pos)))

(defmethod get-byte ((self PosStream))
  "Читает байт из потока, возвращает результат и новое состояние потока"
  (let ((res (super get-byte self)))
    (if (null res) nil	
	(let ((ch (car res))
	      (pos (posstream-pos self)))
	  (cons ch (make-PosStream (posstream-str self) (posstream-index (cdr res))
				   (if (or (= ch (code-char 10)) (= ch (code-char 13)))
				       (next-line pos) (next-char pos))))))))
