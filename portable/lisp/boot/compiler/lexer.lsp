; названия сократил, так как в проекте макс длина символа маленькая 
(defun is-eoi (stream)
  (null stream))

(defun is-eoi-str (stream)
  (>= (SStream-index stream) (string-size (SStream-str stream))))

(defun is-ws (c)
  "Проверяет, является ли символ пробелом."
  (or (eq c #\ ) (eq c (code-char 10)) (eq c (code-char 9)) (eq c (code-char 13))))

(defun is-delim (c)
  "Проверяет, является ли символ разделителем."
  (or (null c) (eq c #\)) (eq c #\() (is-ws c) (eq c #\")))

(defun parse-comment ()
  "Парсит комментарий от ';' до конца строки."
  (parse-app (&&& (parse-elem #\;)
                  (parse-many (parse-pred #'(lambda (c) (not (eq c (code-char 10)))))))
             #'(lambda (x) 'COMMENT)))

(defun skip-ws-comments ()
  "Пропускает пробелы и комментарии."
  (parse-many (parse-or (parse-pred #'is-ws)
                        (parse-comment))))

(defun check-delim (parser)
  "Проверяет разделитель после парсера."
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if res
            (let ((next-char (get-byte (cdr res))))
              (if (or (null next-char) (is-delim (car next-char)))
                  res
                  nil))
            nil))))

(defun parse-lparen ()
  "Парсит открывающую скобку."
  (parse-app (parse-elem #\() #'(lambda (x) (list (code-char 40)))))

(defun parse-rparen ()
  "Парсит закрывающую скобку."
  (parse-app (parse-elem #\)) #'(lambda (x) (list (code-char 41)))))

(defun parse-dot ()
  "Парсит точку."
  (parse-app (check-delim (parse-elem #\.)) #'(lambda (x) (list 'DOT))))

(defun make-num (sign digits)
  "Создает число из знака и списка цифр."
  (let ((num (safe-strtoint (implode digits) 10)))
    (if (eq sign #\-) (- num) num)))

(defun parse-num-tok ()
  "Парсит десятичный числовой токен."
  (parse-app (&&& (parse-optional (parse-elem #\-))
		  (parse-some (parse-pred #'is-digit)))
	     #'(lambda (x) (list (make-num (car x) (cadr x))))))

(defun parse-hex-num ()
  "Парсит числовой токен в 16-ричной системе счисления."
  (parse-app (&&& (parse-elem #\0) (parse-elem #\x)
		  (parse-some (parse-pred #'is-hex-sym)))
	     #'(lambda (x) (list (safe-strtoint (implode (caddr x)) 16)))))

(defun parse-float-tok ()
  "Парсит числовой токен с плавающей точкой."
  (parse-app
   (&&& (parse-optional (parse-elem #\-))
        (parse-or
         (&&& (parse-some (parse-pred #'is-digit))
              (parse-elem #\.)
              (parse-some (parse-pred #'is-digit)))
         (&&& (parse-elem #\.)
              (parse-some (parse-pred #'is-digit)))))
   #'(lambda (x)
       (let* ((sign (car x))
              (parts (cadr x))
              (is-dot-first (eq (car parts) #\.))
              (int-part (if is-dot-first nil (car parts)))
              (frac-part (if is-dot-first (cadr parts) (caddr parts)))
              (int-str (if int-part (implode int-part) "0"))
              (frac-str (implode frac-part))
              (float-str (concat int-str "." frac-str)))
         (list (strtofloat (if sign (concat "-" float-str) float-str)))))))

(defun parse-sym-tok ()
  "Парсит идентификатор (symbol)."
  (parse-app (parse-some (parse-pred #'(lambda (c) (not (is-delim c)))))
	     #'(lambda (x) (list (intern (string-upcase (implode x)))))))

(defun parse-str-char ()
  "Парсит символ строки."
  #'(lambda (stream)
      (let ((res (get-byte stream)))
        (if (null res) nil
            (let ((char (car res)) (next-stream (cdr res)))
              (if (eq char #\\) (get-byte next-stream)
                  (if (eq char #\") nil res)))))))

(defun parse-str-tok ()
  "Парсит строковый токен."
  (parse-app (&&& (parse-elem #\")
                  (parse-many (parse-str-char))
                  (parse-elem #\"))
             #'(lambda (x) (list (implode (cadr x))))))

(defun parse-comma ()
  "Парсит запятую."
  (parse-app (&&& (parse-elem #\,) (parse-optional (parse-elem #\@)))
             #'(lambda (x) (if (cadr x) (list 'COMMA-AT) (list 'COMMA)))))

(defun parse-comma-tok ()
  "Парсит запятую как токен."
    (parse-app (&&& (parse-comma) (parse-rec (parse-token)))
        #'(lambda (x) (append (car x) (cadr x)))))

(defun parse-char-sharp ()
  "Парсит символ после # и обратного слеша."
  (parse-app (&&& (parse-elem #\\) (parse-pred #'(lambda (x) t)))
             #'(lambda (x) (list 'T-CHAR (cadr x)))))

(defun parse-func-sharp ()
  "Парсит цитату функции после #'."
  (parse-app (&&& (parse-elem #\') (parse-rec (parse-token)))
             #'(lambda (x) (append (list 'FUNCTION) (cadr x)))))

(defun parse-sharp ()
  "Парсит токен, начинающийся с #."
  (parse-app (&&& (parse-elem #\#)
                  (parse-or (parse-char-sharp)
                            (parse-func-sharp)
                            (parse-app (parse-rec (parse-token)) #'(lambda (x) (append (list 'SHARP) x)))))
             #'cadr))

(defun parse-quote-tok ()
  "Парсит цитату как токен."
  (parse-app (&&& (parse-elem #\') (parse-rec (parse-token)))
             #'(lambda (x) (append (list 'QUOTE) (cadr x)))))

(defun parse-backq-tok ()
  "Парсит обратную цитату как токен."
  (parse-app (&&& (parse-elem #\`) (parse-rec (parse-token)))
             #'(lambda (x) (append (list 'BACKQUOTE) (cadr x)))))

(defun parse-token ()
  "Парсит токен."
  (parse-or (parse-lparen)
            (parse-rparen)
            (check-delim (parse-hex-num))
            (check-delim (parse-float-tok))
            (check-delim (parse-num-tok))
            (parse-str-tok)
            (parse-sharp)
            (parse-quote-tok)
            (parse-backq-tok)
            (parse-comma-tok)
            (parse-dot)
            (parse-sym-tok)
            (parse-fail "Unknown token")))

(defun lexer (str)
  "Лексер: преобразует строку в список токенов."
  (labels ((lex-loop (s acc)
             (let ((s-ws (cdr (funcall (skip-ws-comments) s))))
               (if (is-eoi-str s-ws) acc
                   (let ((res (funcall (parse-token) s-ws)))
                     (if res
                         (lex-loop (cdr res) (append acc (car res)))
                         nil))))))
    (lex-loop (stream-from-str str) nil)))