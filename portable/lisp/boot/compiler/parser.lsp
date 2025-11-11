(defun is-lparen (tok)
  "Проверяет, является ли токен открывающей скобкой."
  (eq tok (code-char 40)))
(defun is-rparen (tok)
  "Проверяет, является ли токен закрывающей скобкой."
  (eq tok (code-char 41)))
(defun is-dot (tok)
  "Проверяет, является ли токен точкой."
  (eq tok 'DOT))
(defun is-quote (tok)
  "Проверяет, является ли токен одинарной кавычкой (quote)."
  (eq tok 'QUOTE))
(defun is-backquote (tok)
  "Проверяет, является ли токен обратной кавычкой (backquote)."
  (eq tok 'BACKQUOTE))
(defun is-comma (tok)
  "Проверяет, является ли токен запятой (comma)."
  (eq tok 'COMMA))
(defun is-comma-at (tok)
  "Проверяет, является ли токен запятой с собакой (comma-at)."
  (eq tok 'COMMA-AT))
(defun is-function-quote (tok)
  "Проверяет, является ли токен кавычкой функции (function quote)."
  (eq tok 'FUNCTION))
(defun is-t-char (tok)
  "Проверяет, является ли токен символьным литералом."
  (eq tok 'T-CHAR))
(defun is-sharp (tok)
  "Проверяет, является ли токен символом решетки (#)."
  (eq tok 'SHARP))

(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun parse-lisp-token (pred)
  "Создает парсер, который принимает токен, если он удовлетворяет предикату."
  #'(lambda (stream)
      (let ((list (LStream-list stream)))
        (if (null list)
            nil
            (let ((token (car list)))
              (if (funcall pred token)
                  (cons token (make-LStream (cdr list)))
                  nil))))))


(defun parse-list ()
  "Разбор списка, включая точечные пары."
  (parse-app 
      (&&& (parse-lisp-token #'is-lparen)
           (parse-many (parse-rec (parse-s-expression)))
           (parse-optional (&&& (parse-lisp-token #'is-dot)
                                (parse-rec (parse-s-expression))))
           (parse-lisp-token #'is-rparen))
      #'(lambda (res)
          (let ((items (second res))
                (dotted-pair (third res)))
            (if dotted-pair
                (append-dotted items (second dotted-pair))
                items)))))

(defun parse-tchar ()
  "Разбор lisp-символа"
  (parse-app (&&& (parse-lisp-token #'is-t-char)
                  (parse-lisp-token #'(lambda (x) t)))
             #'(lambda (res) (second res))))

(defun parse-array ()
  "Разбор массива #(...)"
  (parse-app (&&& (parse-lisp-token #'is-sharp)
                  (parse-lisp-token #'is-lparen)
                  (parse-many (parse-rec (parse-s-expression)))
                  (parse-lisp-token #'is-rparen))
             #'(lambda (res)
                 (cons 'MAKE-ARRAY (third res)))))

(defun parse-s-expression ()
  "Разбор S-выражения, включая опциональные префиксы."
  (parse-or
   (parse-app
    (&&& (parse-or (parse-lisp-token #'is-quote)
                   (parse-lisp-token #'is-backquote)
                   (parse-lisp-token #'is-comma)
                   (parse-lisp-token #'is-comma-at)
                   (parse-lisp-token #'is-function-quote))
         (parse-rec (parse-s-expression)))
    #'(lambda (res) (list (car res) (cadr res))))
   (parse-tchar)
   (parse-array)
   (parse-lisp-token #'(lambda (tok)
                      (and (not (is-dot tok))
                           (not (is-lparen tok))
                           (not (is-rparen tok))
                           (or (symbolp tok) (integerp tok) (floatp tok) (stringp tok)))))
   (parse-list)))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп и вернуть синтаксическое дерево."
  (let ((tokens (lexer str)))
    (if tokens
        (let* ((stream (stream-from-list tokens))
               (res (funcall (parse-s-expression) stream)))
          (if res
              (let ((rem-stream (cdr res)))
                (if (null (LStream-list rem-stream))
                    (car res)
                    'SYNTAX-ANALYZE-ERROR))
              'SYNTAX-ANALYZE-ERROR))
      'SYNTAX-ANALYZE-ERROR)))