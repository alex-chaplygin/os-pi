(defun token-value (tok)
  "Возвращает значение токена из пары (значение . позиция)"
  (car tok))

(defun token-pos (tok)
  "Возвращает позицию токена из пары (значение . позиция)"
  (cdr tok))

(defun token-elem-p (val)
  "Предикат, проверяющий, что значение токена равно val."
  #'(lambda (tok)
      (eq (token-value tok) val)))

(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun lisp-list ()
  "Разбор списка ( ... ) и точечного списка ( ... . ... )"
  (parse-app
      (&&& (parse-pred (token-elem-p #\())
           (parse-many (parse-rec (lisp-s-expr)))
           (parse-optional (&&& (parse-pred (token-elem-p #\.))
                                (parse-rec (lisp-s-expr))))
           (parse-pred (token-elem-p #\))))
      #'(lambda (res)
          (let ((items (second res))
                (dot-value (third res)))
            (if dot-value
		(if (null items)
		    (throw 'parse-error "lisp-parser: Invalid dotted list: starts with a dot")
		  (append-dotted items (second dot-value)))
              items)))))

(defun lisp-array ()
  "Разбор массива #(...)"
  (parse-app (&&& (parse-pred (token-elem-p 'SHARP)) (lisp-list))
	     #'(lambda (res)
                 (let ((lst (second res)))
                   (if (not (proper-list-p lst))
                       (throw 'parse-error "lisp-parser: Invalid array: dotted list is not allowed")
                     (list-to-array lst))))))

(defun is-atom-p (tok)
  "Проверить, что это атом"
  (let ((val (token-value tok)))
    (and (not (eq val #\()) (not (eq val #\))) (not (eq val #\.))
	 (or (symbolp val) (integerp val) (floatp val) (stringp val) (charp val)))))

(defun lisp-s-expr ()
  "Разбор s-выражения"
  (parse-or
   (parse-app
    (&&& (parse-or (parse-pred (token-elem-p 'QUOTE))
                   (parse-pred (token-elem-p 'BACKQUOTE))
                   (parse-pred (token-elem-p 'COMMA))
                   (parse-pred (token-elem-p 'COMMA-AT))
                   (parse-pred (token-elem-p 'FUNCTION)))
         (parse-rec (lisp-s-expr)))
    #'(lambda (res) (list (token-value (car res)) (cadr res))))
   (lisp-array)
   (parse-app (parse-pred #'is-atom-p) #'token-value)
   (lisp-list)))

(defun check-parens (tokens)
  "Проверяет баланс скобок в списке токенов."
  (let ((balance 0)
	(last-pos nil))
    (dolist (tok tokens)
      (setf last-pos (token-pos tok))
      (let ((val (token-value tok)))
	(cond
	  ((eq val #\() (setf balance (++ balance)))
	  ((eq val #\)) (setf balance (-- balance))))
	(if (< balance 0)
	    (throw 'parse-error (concat "lisp-parser: Found an extra closing parenthesis at " (pos-to-str last-pos)))
	  nil)))
    (if (!= balance 0)
	(throw 'parse-error (concat "lisp-parser: Unbalanced parentheses. Expected a closing parenthesis, last position: "
				   (pos-to-str last-pos)))
      nil)))

(defun pos-to-str (pos)
  (concat "(" (inttostr (pos-line pos)) " . " (inttostr (pos-char pos)) ")"))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп. Возвращает дерево разбора или выбрасывает ошибку."
  (catch 'parse-error
    (let* ((tokens (lisp-lexer str)))
      (if (not (or (null tokens) (pairp tokens)))
          "lisp-parser-from-lexer: unexpected result"
        (progn
	  (check-parens tokens)
	  (let* ((stream (stream-from-list tokens))
		 (res (funcall (lisp-s-expr) stream)))
	    (if (not res)
		"lisp-parser: Syntax error"
	      (let ((rest-stream (cdr res)))
		(if (not (stream-empty-p rest-stream))
		    (concat "lisp-parser: Unexpected tokens after expression, starting from "
			    (pos-to-str (token-pos (stream-peek rest-stream))))
		  (car res))))))))))

