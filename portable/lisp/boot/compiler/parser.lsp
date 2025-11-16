(defun lisp-s-expr () nil)

(defun append-dotted (list-part dotted-part)
  "Рекурсивно присоединяет `dotted-part` в конец `list-part` для создания точечного списка."
  (if (null list-part)
      dotted-part
      (cons (car list-part) (append-dotted (cdr list-part) dotted-part))))

(defun lisp-list ()
  "Разбор списка, включая точечные пары."
  (parse-app 
      (&&& (parse-elem #\()
           (parse-many (parse-rec (lisp-s-expr)))
           (parse-optional (&&& (parse-elem #\.)
                                (parse-rec (lisp-s-expr))))
           (parse-elem #\)))
      #'(lambda (res)
          (let ((items (second res))
                (dotted-pair (third res)))
            (if dotted-pair
                (append-dotted items (second dotted-pair))
                items)))))

(defun lisp-array ()
  "Разбор массива #(...)"
  (parse-app (&&& (parse-elem 'SHARP) (lisp-list)) #'(lambda (res)
						       (list-to-array (second res)))))
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
		   (and (!= x #\() (!= x #\)) (!= x #\.)
			(or (symbolp x) (integerp x) (floatp x) (stringp x) (charp x)))))
   (lisp-list)))	    

(defun parse-lisp (str)
  "Распознать строку с программой Лисп"
  (let ((res (funcall (lisp-s-expr) (stream-from-list (lisp-lexer str)))))
    (if res (car res) res)))
