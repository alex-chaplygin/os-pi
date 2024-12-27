; функции для работы со строками
(defun search (c str)
  "Найти в строке str позицию первого вхождения символа c"
  (let ((pos nil)
	(size (string-size str)))
    (for i 0 size
	 (when (= (code-char (char str (- size i 1))) c)
	   (setq pos (- size i 1))))
    pos))

(defun search-back (c str)
  "Найти в строке str позицию последнего вхождения символа c"
  (let ((pos nil)
	(size (string-size str)))
    (for i 0 size
	 (when (= (code-char (char str i)) c)
	   (setq pos i)))
    pos))

(defun split (del str)
  "Разделить строку str на элементы по символу разделителю del"
  (let ((f (search del str)))
    (if (null f) (list str)
	(cons (subseq str 0 f)
	      (if (= f (- (string-size str) 1)) (list "")
		  (split del (subseq str (+ f 1) (string-size str))))))))

(defun implode (char-list)
  "Преобразует список печатных символов в строку"
  (let* ((len (list-length char-list))
	 (str (make-string len #\ )))
    (for i 0 len
	 (sets str i (nth char-list i)))
    str))

(defun explode (str)
  "Создаёт список из печатных символов строки"
  (let ((out-list NIL)
	(iter (string-size str)))
    (while (> iter 0)
      (setq iter (- iter 1))
	 (setq out-list (cons (char str iter) out-list)))
    out-list))

(defun is-alpha (sym)
  "Предикат проверки на букву"
  (let ((c (char-code sym)))
    (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))))

(defun is-digit (sym)
  "Предикат проверки на цифру"
  (let ((c (char-code sym)))
    (and (>= c 48) (<= c 57))))
