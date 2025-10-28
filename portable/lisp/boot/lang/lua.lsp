

(defun parse-var()
  (parse-app (parse-some (parse-pred #'is-alpha))
	     #'(lambda (char-list) (intern (implode char-list)))))

(defun parse-num()
  (parse-or parse-decimal parse-hex))

(defun parse-op()
  (parse-or (parse-elem #\-) (parse-elem #\+) (parse-elem #\*) (parse-elem #\/) (parse-elem #\=)))

(defun parse-lexem()
  (parse-or parse-var parse-num parse-op))

(defun parse-lexems()
  (parse-some (parse-lexem)))
