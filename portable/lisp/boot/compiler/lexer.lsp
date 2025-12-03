(defun same-pos-p (pos1 pos2)
  "Сравнивает две позиции (списки из двух чисел)."
  (and pos1 pos2
       (= (car pos1) (car pos2))
       (= (second pos1) (second pos2))))


(defun throw-error (message &rest pos)
  "Бросает ошибку с позицией"
  (if pos
      (raise 'parse-error (list (car pos) message))
  #'(lambda (stream)
        (let ((pos (PosStream-pos stream)))
          (raise 'parse-error (list pos message))))))

(defun parse-float ()
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
          (let ((sign (car (car res)))          ; результат sign->...
                (int-part (second (car res)))   ; результат int-part->...
                (frac-part (forth (car res)))   ; результат frac-part->... ( напрямую не вышло, область видимости не совпала)
                (rest-stream (cdr res)))
            (if (and (null int-part) (null frac-part))
                (if sign
                    (funcall (throw-error "lisp-lexer: WARNING: got dot with sign") stream)
                    (cons #\. rest-stream))     ; ← возвращаем точку как символ для точечных пар
                (let* ((float-str (concat (implode int-part) "." (implode frac-part)))
                       (num (strtofloat (if sign (concat "-" float-str) float-str))))
                  (if num
                      (cons num rest-stream)
                      (funcall (throw-error "lisp-lexer: invalid float number") stream)))))))))

;; Вспомогательный предикат
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
  "Разбор экранированной последовательности"
    (&&& s1->(parse-elem #\\)
         s-> (parse-or (parse-elem char)
                        (parse-app (parse-return nil)
                              #'(lambda (x) (throw-error "lisp-lexer: unexpected end of escape sequence"))))
    return (lambda (parts) (list value))))

(defun parse-string ()
  "Разбор строки в двойных кавычках"
  (parse-app ; оставил parse-app так как с &&& функция будет в 5 раз длиннее, ну либо опять ошибка области видимости
    (&&& (parse-elem #\")
         (parse-many (parse-or (parse-escape #\n (code-char 0xa)) ; \n
			       (parse-escape #\\ #\\)
			       (parse-escape #\" #\")
              (parse-pred #'(lambda (sym) (and (!= sym #\") (!= sym #\\))))))
	        (parse-or (parse-elem #\")
		          (throw-error "lisp-lexer: unterminated string or unexpected end of escape sequence")))
    #'(lambda (parts) (implode (second parts)))))

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
                #'(lambda (stream) (unless (end-of-stream stream) (funcall (throw-error "lisp-lexer: Unknown token") stream)))
          ))



(defun lisp-lexer (str)
  "Лексический анализатор Common Lisp. Строка преобразуется в список лексем"
  (let ((r (funcall (parse-sep (parse-with-pos (lisp-token)) (lisp-ws-or-comment))
		      (stream-from-str str))))
    (if (not r)
        (throw-error "lisp-lexer: unknown lexer error")
      (car r))))

