(defun parse-hex ()
  "Разбор шестнадцатеричного числа вида 0xFF"
  (parse-app
    (&&& (parse-elem #\0)
         (parse-elem #\x)
         (parse-pred #'is-hex-sym)
         (parse-many (parse-pred #'is-hex-sym)))
    #'(lambda (parts)
        (strtoint (implode (cons (third parts) (forth parts))) 16))))

(defun parse-decimal ()
  "Разбор десятичного числа (поддерживает -123)"
  (parse-app
    (&&& (parse-optional (parse-elem #\-))   ; ← необязательный минус
         (parse-pred #'is-digit)
         (parse-many (parse-pred #'is-digit)))
    #'(lambda (parts)
	(let ((num (strtoint (implode (cons (second parts) (third parts))) 10)))
            (if (car parts) (- num) num)))))

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

;; (defun parse-tfunction ()
;;   "Разбор функции вида #'name"
;;   (parse-app
;;     (&&& (skip-spaces)
;;          (parse-elem #\#)
;;          (parse-elem #\')
;;          (parse-s))  ; ← зависит от parse-s (циклическая зависимость — допустима)
;;     #'(lambda (parts) (list 'FUNCTION (fourth parts)))))

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

;; (defun parse-tarray ()
;;   "Разбор массива #(...)"
;;   (parse-app
;;     (&&& (parse-elem #\#) (parse-list))
;;     #'(lambda (parts) (list-to-array (second parts)))))

(defun lisp-token ()
  "Лексема языка Лисп"
  (parse-or (parse-elem #\()
            (parse-elem #\))
            (parse-hex)
            (parse-decimal)
;;            (parse-float)
            (parse-string)
	    (parse-app (&&& (parse-elem #\#) (parse-elem #\\) #'get-byte) #'third) ;; одиночный символ
            (parse-app (&&& (parse-elem #\#) (parse-elem #\')) (parse-return 'FUNCTION))
            (parse-app (parse-elem #\#) (parse-return 'SHARP))
            (parse-app (parse-elem #\') (parse-return 'QUOTE))
            (parse-app (parse-elem #\`) (parse-return 'BACKQUOTE))
            (parse-app (&&& (parse-elem #\,) (parse-elem #\@)) (parse-return 'COMMA-AT))
            (parse-app (parse-elem #\,) (parse-return 'COMMA))
            (parse-elem #\.)
            (lisp-symbol)
	    ;;            #'(lambda (stream) (throw 'parse-error "Unknown token"))))
	    ))

(defun lisp-lexer (str)
  "Лексический анализатор Common Lisp. Строка преобразуется в список лексем"
  (let ((r (catch 'parse-error
	     (funcall (parse-sep (lisp-token) (parse-many (parse-pred #'lisp-separator)))
		      (stream-from-str str)))))
    (if r (car r) nil)))
