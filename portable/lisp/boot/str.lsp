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

(defun str-repl (size ch)
  "Создает строку, где символ ch повторяется size раз"
  (cond
    ((= size 0) "")
    ((= size 1) ch)
    (t (concat ch (str-repl (- size 1) ch)))))
