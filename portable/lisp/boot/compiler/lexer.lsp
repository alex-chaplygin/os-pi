(defun parse-float()
  "Парсит числовой токен с плавающей точкой."
  (parse-app
   (&&& (parse-optional (parse-elem #\-))
        (parse-many (parse-pred #'is-digit))
	(parse-elem #\.)
	(parse-many (parse-pred #'is-digit)))
   #'(lambda (x)
       (let* ((sign (car x))
              (int-part (second x))
              (frac-part (forth x))
              (float-str (concat (implode int-part) "." (implode frac-part))))
	 (if (and (null int-part) (null frac-part)) #\.
	     (strtofloat (if sign (concat "-" float-str) float-str)))))))

;; Вспомогательный предикат
(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\% #\! #\^ #\~) sym))

(defun lisp-separator (sym)
  "Разделители в Лиспе"
  (contains (list (code-char 9) (code-char 10) #\  (code-char 0xa)) sym))

(defun lisp-symbol ()
  "Разбор символа (идентификатора)"
  (parse-app
    (&&& (parse-or (parse-pred #'is-alpha)
                   (parse-pred #'is-lisp-symbol))
         (parse-many (parse-or (parse-pred #'is-alpha)
                               (parse-pred #'is-digit)
                               (parse-pred #'is-lisp-symbol))))
    #'(lambda (parts)
        (intern (implode (map #'(lambda (char) (toupper char))
                              (cons (car parts) (second parts))))))))

(defun parse-escape (char value)
  "Разбор экранированной последовательности"
  (parse-app
    (&&& (parse-elem #\\) (parse-elem char))
    #'(lambda (parts) (list value))))

(defun parse-string ()
  "Разбор строки в двойных кавычках"
  (parse-app
    (&&& (parse-elem #\")
         (parse-many (parse-or (parse-escape #\n (code-char 0xa))
                               (parse-pred #'(lambda (sym) (!= sym #\")))))
         (parse-elem #\"))
    #'(lambda (parts) (implode (second parts)))))

(defun lisp-token ()
  "Лексема языка Лисп"
  (parse-or (parse-elem #\()
            (parse-elem #\))
            (parse-float)
            (parse-hex)
            (parse-decimal)
            (parse-string)
	    (parse-app (&&& (parse-elem #\#) (parse-elem #\\) #'get-byte) #'third) ;; одиночный символ
            (parse-app (&&& (parse-elem #\#) (parse-elem #\')) (parse-return 'FUNCTION))
            (parse-app (parse-elem #\#) (parse-return 'SHARP))
            (parse-app (parse-elem #\') (parse-return 'QUOTE))
            (parse-app (parse-elem #\`) (parse-return 'BACKQUOTE))
            (parse-app (&&& (parse-elem #\,) (parse-elem #\@)) (parse-return 'COMMA-AT))
            (parse-app (parse-elem #\,) (parse-return 'COMMA))
            (lisp-symbol)
	    ;;            #'(lambda (stream) (throw 'parse-error "Unknown token"))))
	    ))

(defun parse-with-pos (parser)
  "Парсер-комбинатор, который добавляет начальную позицию к результату."
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if res (cons (cons (car res) (PosStream-pos stream)) (cdr res))
	    nil))))

(defun lisp-lexer (str)
  "Лексический анализатор Common Lisp. Строка преобразуется в список лексем"
  (let ((r (funcall (parse-sep (parse-with-pos (lisp-token)) (parse-many (parse-pred #'lisp-separator)))
		      (stream-from-str str))))
    (if r (car r) nil)))
