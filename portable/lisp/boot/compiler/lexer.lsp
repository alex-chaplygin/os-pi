;; глобальные переменные для хранения состояния (через defvar не работает в такой области видимости)
(intern "*STREAM*")
(intern "*LAST-GOOD-RESULT*")
(intern "*LAST-THROW-POS*")

(setf *STREAM* nil)
(setf *LAST-GOOD-RESULT* nil)
(setf *LAST-THROW-POS* nil)

(defun same-pos-p (pos1 pos2)
  "Сравнивает две позиции (списки из двух чисел)."
  (and pos1 pos2
       (= (car pos1) (car pos2))
       (= (second pos1) (second pos2))))

(defun throw-error (type message)
  "Бросает ошибку только если позиция отличается от предыдущей."
  (let* ((pos (PosStream-pos *STREAM*))
         (should-throw (not (same-pos-p pos *LAST-THROW-POS*))))
    (if should-throw
        (progn
          (setf *LAST-THROW-POS* pos)
          (print (concat "Error at ("
                         (concat (inttostr (car pos)) " . " (inttostr (second pos)))
                         "); Last succesfuly parsed symbol: ")
                 *LAST-GOOD-RESULT*)
          (throw type message))
      nil)))


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
              (frac-part (forth x)))
	 (if (and (null int-part) (null frac-part))
	     (if sign
		 (throw-error 'parse-error "lisp: WARNING: got dot with sign")
	       #\.)
	   (let* ((float-str (concat (implode int-part) "." (implode frac-part)))
		  (num (strtofloat (if sign (concat "-" float-str) float-str))))
	     (if num num
		 (throw-error 'parse-error "lisp: invalid float number"))))))))

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
  (parse-app
    (&&& (parse-or (parse-pred #'is-alpha)
                   (parse-pred #'is-lisp-symbol))
         (parse-many (parse-or (parse-pred #'is-alpha)
                               (parse-pred #'is-digit)
                               (parse-pred #'is-lisp-symbol))))
    #'(lambda (parts)
	(let ((res (intern (implode (map #'(lambda (char) (toupper char))
					 (cons (car parts) (second parts)))))))
	  (if res res
	      (throw-error 'parse-error "lisp: invalid symbol"))))))

(defun parse-escape (char value)
  "Разбор экранированной последовательности"
  (parse-app
    (&&& (parse-elem #\\)
         (parse-or (parse-elem char)
		   (parse-app (parse-return nil)
			      #'(lambda (x) (throw-error 'parse-error "lisp: unexpected end of escape sequence")))))
    #'(lambda (parts) (list value))))

(defun parse-string ()
  "Разбор строки в двойных кавычках"
  (parse-app
    (&&& (parse-elem #\")
         (parse-many (parse-or (parse-escape #\n (code-char 0xa))
                               (parse-pred #'(lambda (sym) (!= sym #\")))))
	 (parse-or (parse-elem #\")
		   (parse-app (parse-optional (parse-elem (code-char 0)))
			      #'(lambda (x) (throw-error 'parse-error "lisp: unterminated string")))))
    #'(lambda (parts) (implode (second parts)))))

(defun parse-comment-separator ()
  "Парсит комментарий, который начинается с ; и до конца строки"
  (&&& (parse-elem #\;)
       (parse-many (parse-pred #'(lambda (c) (not (or (= c (code-char 10)) (= c (code-char 0xa)))))))))

(defun lisp-ws-or-comment ()
  "Парсер для пробельных символов и комментариев"
  (parse-many (parse-or (parse-pred #'lisp-separator)
                        (parse-comment-separator))))

(defun make-tok-err ()
  "Создает ошибку разбора (через лямбду объединить с lisp-token не получается)"
  (parse-app
    (parse-pred #'(lambda (x) t))
    #'(lambda (x)
        (throw-error 'parse-error "lisp: Unknown token"))))

(defun lisp-token ()
  "Лексема языка Лисп"
  (parse-app (parse-or (parse-elem #\()
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
                      (make-tok-err)
                      )
     #'(lambda (tok)
        (setf *LAST-GOOD-RESULT* tok)
        (setf *LAST-THROW-POS* nil)
        tok)))


(defun lisp-lexer (str)
  "Лексический анализатор Common Lisp. Строка преобразуется в список лексем"
  (setf *STREAM* (stream-from-str str))
  (setf *LAST-GOOD-RESULT* nil)
  (setf *LAST-THROW-POS* nil)
  (let ((r (funcall (parse-sep (lisp-token) (lisp-ws-or-comment))
                    *STREAM*)))
    (if (not r)
        (throw-error 'parse-error "lisp: unknown lexer error")
      (car r))))
