(defun parser-error (message pos)
  "Бросает ошибку с позицией"
  (raise 'parse-error (list pos message)))

(defun lisp-s-expr () nil)

(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun token-value (tok)
  "Возвращает значение токена из пары (значение . позиция)"
  (car tok))

(defun token-pos (tok)
  "Возвращает позицию токена из пары (значение . позиция)"
  (cdr tok))

(defun lisp-elem (val)
  "Парсер ожидающий значение val"
  (parse-pred #'(lambda (tok) (eq (token-value tok) val))))

(defun lisp-list ()
  "Разбор списка, включая точечные пары."
  (&&& start->(lisp-elem #\()
       items->(parse-many (parse-rec (lisp-s-expr)))
       dot-part->(parse-optional (&&& (lisp-elem #\.)
				      (parse-rec (lisp-s-expr))))
       (lisp-elem #\))
       return (if dot-part
		  (if (null items)
		      (parser-error "lisp-parser: Invalid dotted list: starts with a dot." (token-pos start))
		      (cons (append-dotted (map #'car items) (token-value (second dot-part))) (cdar items)))
		  (if items (cons (map #'car items) (cdar items))
		      (cons nil (token-pos start))))))

(defun lisp-array ()
  "Разбор массива #(...)"
  (&&& (lisp-elem 'SHARP) list->(lisp-list) return (cons (list-to-array (token-value list)) (token-pos list))))

(defun lisp-s-expr ()
  "Разбор s-выражения"
  (parse-or
   (&&& op->(parse-or (lisp-elem 'QUOTE)
		      (lisp-elem 'BACKQUOTE)
		      (lisp-elem 'COMMA)
		      (lisp-elem 'COMMA-AT)
		      (lisp-elem 'FUNCTION))
	expr->(parse-rec (lisp-s-expr))
	return (cons (list (token-value op) (token-value expr)) (token-pos op)))
   (lisp-array)
   (parse-pred #'(lambda (x)
		   (let ((val (token-value x)))
		     (and (!= val #\() (!= val #\)) (!= val #\.)
			  (or (symbolp val) (integerp val) (floatp val) (stringp val) (charp val))))))
   (lisp-list)))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп"
  (let ((res (funcall (lisp-s-expr) (stream-from-list (lisp-lexer str)))))
    (if res (caar res) res)))
