; функции для работы со строками
(defun search (c str)
  (let ((pos nil)
	(size (string-size str)))
    (for i 0 size
	 (when (= (code-char (char str (- size i 1))) c)
	   (setq pos (- size i 1))))
    pos))
