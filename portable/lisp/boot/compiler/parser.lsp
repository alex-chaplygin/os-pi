(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun lisp-list ()
  "Разбор списка ( ... ) и точечного списка ( ... . ... )"
  (parse-app 
      (&&& (parse-elem #\()
           (parse-many (parse-rec (lisp-s-expr)))
           (parse-optional (&&& (parse-elem #\.)
                                (parse-rec (lisp-s-expr))))
           (parse-or (parse-elem #\))
                     (parse-app (parse-return nil)
                                #'(lambda (x)
                                    nil)))) 
      #'(lambda (res)
          (let ((items (second res))
                (dot-value (third res)))
            (cond
              ((and dot-value (null (second dot-value)))
               nil) 
              (dot-value
               (append-dotted items (second dot-value)))
              (t items))))))

(defun lisp-array ()
  "Разбор массива #(...)"
  (parse-app (&&& (parse-elem 'SHARP) (lisp-list)) #'(lambda (res)
						       (list-to-array (second res)))))


(defun token-elem-p (val)
  "Проверить, что значение токена равно val (с распаковкой информации о позиции)"
  #'(lambda (tok)
      (= (token-value tok) val)))

(defun is-atom-p (tok)
  "Проверить, что это атом"
  #'(lambda (x)
      (let ((val (token-value x)))
	(and (!= val #\() (!= val #\)) (!= val #\.)
	     (or (symbolp val) (integerp val) (floatp val) (stringp val) (charp val))))))

   

(defun lisp-s-expr ()
  "Разбор s-выражения"
  (parse-or
   (parse-app
    (&&& (parse-or (parse-elem 'QUOTE)
                   (parse-elem 'BACKQUOTE)
                   (parse-elem 'COMMA)
                   (parse-elem 'COMMA-AT)
                   (parse-elem 'FUNCTION))
         (parse-rec (lisp-s-expr)))
    #'(lambda (res) (list (car res) (cadr res))))
   (lisp-array)
   (parse-pred #'(lambda (x)
                   (cond
                     ((= x #\.) nil)
                     ((or (= x #\() (= x #\))) nil)
                     (t (or (symbolp x) (integerp x) (floatp x) (stringp x) (charp x)))))) 
   (lisp-list)))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп. Возвращает дерево разбора или выбрасывает ошибку."
  (catch 'parse-error
    (let* ((tokens (lisp-lexer str)))
      (if (not (or (null tokens) (pairp tokens)))
          (throw-error 'parse-error "lisp-parser: Unexpected lexer result")
        (let* ((stream (stream-from-list tokens))
               (res (funcall (lisp-s-expr) stream)))
          (if (not res)
              (throw-error 'parse-error "lisp-parser: Syntax error")
            (let ((rest-stream (cdr res)))
              (if (not (stream-empty-p rest-stream))
                  (throw-error 'parse-error "lisp-parser: Unexpected tokens after expression")
                (car res)))))))))

