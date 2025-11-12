(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun parse-list ()
  "Разбор списка, включая точечные пары."
  (parse-app 
      (&&& (parse-elem (code-char 40))
           (parse-many (parse-rec (parse-s-expression)))
           (parse-optional (&&& (parse-elem 'DOT)
                                (parse-rec (parse-s-expression))))
           (parse-elem (code-char 41)))
      #'(lambda (res)
          (let ((items (second res))
                (dotted-pair (third res)))
            (if dotted-pair
                (append-dotted items (second dotted-pair))
                items)))))

(defun parse-tchar ()
  "Разбор lisp-символа"
  (parse-app (&&& (parse-elem 'T-CHAR)
                  (parse-pred #'(lambda (x) t)))
             #'(lambda (res) (second res))))

(defun parse-array ()
  "Разбор массива #(...)"
  (parse-app (&&& (parse-elem 'SHARP)
                  (parse-elem (code-char 40))
                  (parse-many (parse-rec (parse-s-expression)))
                  (parse-elem (code-char 41)))
             #'(lambda (res)
                 (cons 'MAKE-ARRAY (third res)))))

(defun parse-s-expression ()
  "Разбор S-выражения, включая опциональные префиксы."
  (parse-or
   (parse-app
    (&&& (parse-or (parse-elem 'QUOTE)
                   (parse-elem 'BACKQUOTE)
                   (parse-elem 'COMMA)
                   (parse-elem 'COMMA-AT)
                   (parse-elem 'FUNCTION))
         (parse-rec (parse-s-expression)))
    #'(lambda (res) (list (car res) (cadr res))))
   (parse-tchar)
   (parse-array)
   (parse-pred #'(lambda (tok)
                      (and (not (eq tok 'DOT))
                           (not (eq tok (code-char 40)))
                           (not (eq tok (code-char 41)))
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