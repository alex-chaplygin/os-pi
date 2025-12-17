(defun lisp-error (message &rest end)
  "Бросает ошибку с позицией"
  #'(lambda (stream)
      (let ((pos (PosStream-pos stream))
	    (con (if end (end-of-stream stream) nil)))
	(unless con
	  (raise 'parse-error (list pos message))))))

(defun parse-float()
  "Парсит числовой токен с плавающей точкой."
  #'(lambda (stream)
    (let ((res (funcall
                 (&&& sign->(parse-optional (parse-elem #\-))
                      int-part->(parse-many (parse-pred #'is-digit))
                      (parse-elem #\.)
                      frac-part->(parse-many (parse-pred #'is-digit)))
                 stream)))
      (if (null res)
          nil
          (let ((sign (car (car res)))
                (int-part (second (car res)))
                (frac-part (forth (car res)))
                (rest-stream (cdr res)))
            (if (and (null int-part) (null frac-part))
                (if sign
                    (funcall (lisp-error "lisp-lexer: got dot with sign") stream)
                    (cons #\. rest-stream))
                (let* ((float-str (concat (implode int-part) "." (implode frac-part)))
                       (num (strtofloat (if sign (concat "-" float-str) float-str))))
                  (if num
                      ;Если дробная часть пустая (случай "13." или "-2."), то будет проверка на мусорный ввод
                      (if (and (null frac-part)
                               (funcall (parse-pred #'is-alpha) rest-stream))
                          (funcall (lisp-error "lisp-lexer: invalid float number") stream)
                          ;Иначе всё ок (например, там пробел, скобка или конец строки)
                          (cons num rest-stream))
                      (funcall (lisp-error "lisp-lexer: invalid float number") stream)))))))))

;Вспомогательный предикат
(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\% #\! #\^ #\~) sym))

(defun lisp-separator (sym)
  "Разделители в Лиспе"
  (contains (list (code-char 9) (code-char 10) #\  (code-char 0xa)) sym))

(defun is-delimiter (sym)
  "Проверяет, является ли символ разделителем (пробел, новая строка, скобка и т.д.)"
  (or (lisp-separator sym) (= sym #\() (= sym #\)) ))


(defun lisp-symbol ()
  "Разбор символа (идентификатора)"
    (&&& s1->(parse-or (parse-pred #'is-alpha)
                   (parse-pred #'is-lisp-symbol))
         s->(parse-many (parse-or (parse-pred #'is-alpha)
                               (parse-pred #'is-digit)
                               (parse-pred #'is-lisp-symbol)))
          return (intern (implode (map #'(lambda (char) (toupper char)) (cons s1 s))))))

(defun parse-escape (char value)
  "Разбор валидной экранированной последовательности (без обработки ошибок)"
  (&&& (parse-elem #\\)
       (parse-elem char)
       return value))

(defun parse-string ()
  "Разбор строки в двойных кавычках"
  (&&& (parse-elem #\")  ; пропуск открывающей кавычки
       chars->(parse-many 
                (parse-or 
                  (parse-escape #\n (code-char 0xa))
                  (parse-escape #\\ #\\)
                  (parse-escape #\" #\")
                  (&&& (parse-elem #\\)
                       (lisp-error "lisp-lexer: unexpected end of escape sequence"))
                  (parse-pred #'(lambda (sym) (and (!= sym #\") (!= sym #\\))))))
       
       ;пропуск закрывающей кавычки, если ее нету - строка не закрыта
       (parse-or (parse-elem #\")
                 (lisp-error "lisp-lexer: unterminated string"))
       return (implode chars)))

(defun parse-comment-separator ()
  "Парсит комментарий, который начинается с ; и до конца строки"
  (&&& (parse-elem #\;)
       (parse-many (parse-pred #'(lambda (c) (not (or (= c (code-char 10)) (= c (code-char 0xa)))))))))

(defun lisp-ws-or-comment ()
  "Парсер для пробельных символов и комментариев"
  (parse-many (parse-or (parse-pred #'lisp-separator)
                        (parse-comment-separator))))

(defun parse-with-pos (parser)
  "Парсер-комбинатор, который добавляет начальную позицию к результату."
  #'(lambda (stream)
      (let ((pos (PosStream-pos stream))
            (res (funcall parser stream)))
        (if res
            (cons (cons (car res) pos) (cdr res))
          nil))))

(defun lisp-token ()
  "Лексема языка Лисп"
      (parse-or (parse-elem #\()
                (parse-elem #\))
                (parse-float)
                (parse-hex)
                (parse-decimal)
                (parse-string)
                (&&& (parse-elem #\#) (parse-elem #\\) c-> #'get-byte return c) ;; одиночный символ
                (&&& (parse-elem #\#) (parse-elem #\') return 'FUNCTION)
                (&&& (parse-elem #\#) return 'SHARP)
                (&&& (parse-elem #\') return 'QUOTE)
                (&&& (parse-elem #\`) return 'BACKQUOTE)
                (&&& (parse-elem #\,) (parse-elem #\@) return 'COMMA-AT)
                (&&& (parse-elem #\,) return 'COMMA)
                (lisp-symbol)
                (lisp-error "lisp-lexer: Unknown token" t)
          ))



(defun lisp-lexer (str)
  "Лексический анализатор Common Lisp. Строка преобразуется в список лексем"
  (let ((r (funcall (parse-sep (parse-with-pos (lisp-token)) (lisp-ws-or-comment))
		      (stream-from-str str))))
    (if (not r)
        (lisp-error "lisp-lexer: unknown lexer error")
      (car r))))

