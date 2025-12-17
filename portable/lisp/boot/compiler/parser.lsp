(defun parser-error (message pos)
  "Бросает ошибку с позицией"
      (raise 'parse-error (list pos message)))

(defun token-value (tok)
  "Возвращает значение токена из пары (значение . позиция)"
  (car tok))

(defun token-pos (tok)
  "Возвращает позицию токена из пары (значение . позиция)"
  (cdr tok))

(defun token-elem-p (val)
  "Проверяет, что значение токена равно val."
  #'(lambda (tok)
      (eq (token-value tok) val)))

(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))


(defun lisp-list ()
  (&&& open->(parse-pred (token-elem-p #\())
       items->(parse-many (parse-rec (lisp-s-expr)))
       dot-part->(parse-optional (&&& (parse-pred (token-elem-p #\.))
                                      (parse-rec (lisp-s-expr))))
       close->(parse-pred (token-elem-p #\)))
       return (if dot-part
                  (if (null items)
                      (parser-error "lisp-parser: Invalid dotted list: starts with a dot."
                                         (token-pos open))
                    (append-dotted items (second dot-part)))
                items)))

(defun lisp-array ()
  (&&& sharp->(parse-pred (token-elem-p 'SHARP))
       lst->(lisp-list)
       return (if (not (proper-list-p lst))
                  (parser-error "lisp-parser: Invalid array: dotted list is not allowed."
                                     (token-pos sharp))
                (list-to-array lst))))

(defun is-atom-p (tok)
  "Проверить, что это атом"
  (let ((val (token-value tok)))
    (and (not (eq val #\()) (not (eq val #\))) (not (eq val #\.))
	 (or (symbolp val) (integerp val) (floatp val) (stringp val) (charp val)))))

(defun lisp-s-expr ()
  "Разбор s-выражения"
  (parse-or
    (&&& op->(parse-or (parse-pred (token-elem-p 'QUOTE))
                       (parse-pred (token-elem-p 'BACKQUOTE))
                       (parse-pred (token-elem-p 'COMMA))
                       (parse-pred (token-elem-p 'COMMA-AT))
                       (parse-pred (token-elem-p 'FUNCTION)))
         expr->(parse-rec (lisp-s-expr))
         return (list (token-value op) expr))
    (lisp-array)
    (parse-app (parse-pred #'is-atom-p) #'token-value)
    (lisp-list)))

(defun parse-lisp (str)
  (catch 'parse-error
    (let* ((tokens (lisp-lexer str)))
      (unless (pairp tokens)
        ;; Лексер сломался — используем (1 1) как fallback. Но вообще ошибка вряд ли возможна.
        (parser-error "lisp-parser: unexpected result of lexer. Stream may be damaged." (list 1 1)))
      (let* ((stream (stream-from-list tokens))
             (res (funcall (lisp-s-expr) stream)))
        (if (null res)
            (parser-error "lisp-parser: Syntax error" (token-pos (car tokens)))
          (let ((rest-stream (cdr res)))
            (if (stream-empty-p rest-stream)
                (car res)
              (parser-error "lisp-parser: Unexpected tokens after expression."
                           (token-pos (stream-peek rest-stream))))))))))                                 