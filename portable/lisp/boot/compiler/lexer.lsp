
(defun is-eoi (stream)
  (null stream))

(defun is-eoi-str (stream)
  "Проверяет, достигнут ли конец потока"
  (>= (SStream-index stream) (string-size (SStream-str stream))))

(defun is-whitespace (c)
  (or (eq c #\ ) (eq c (code-char 10)) (eq c (code-char 9))))

(defun is-delimiter (c)
  (or (null c) (eq c #\)) (eq c #\() (is-whitespace c) (eq c #\")))

(defun parse-lparen ()
  (parse-app (parse-elem #\() #'(lambda (x) (list (code-char 40)))))

(defun parse-rparen ()
  (parse-app (parse-elem #\)) #'(lambda (x) (list (code-char 41)))))

(defun parse-dot ()
  (parse-app (parse-elem #\.) #'(lambda (x) (list 'DOT))))

(defun make-number (sign digits)
  (let ((num (safe-strtoint (implode digits) 10)))
    (if (eq sign #\-) (- num) num)))

(defun parse-number-token ()
  (parse-app (&&& (parse-optional (parse-elem #\-))
		  (parse-some (parse-pred #'is-digit)))
	     #'(lambda (x) (list (make-number (car x) (cadr x))))))

(defun parse-hex-number ()
  (parse-app (&&& (parse-elem #\0) (parse-elem #\x)
		  (parse-some (parse-pred #'is-hex-sym)))
	     #'(lambda (x) (list (safe-strtoint (implode (caddr x)) 16)))))

(defun parse-float-token ()
  (parse-app (&&& (parse-elem #\.)
                  (parse-some (parse-pred #'is-digit)))
             #'(lambda (x) (list (strtofloat (concat "." (implode (cadr x))))))))

(defun parse-float-token-2 ()
    (parse-app (&&& (parse-number-token)
                    (parse-elem #\.)
                    (parse-some (parse-pred #'is-digit)))
         #'(lambda (x) (list (strtofloat (concat (numbertostr (caar x)) "." (implode (caddr x))))))))

(defun parse-symbol-token ()
  (parse-app (parse-some (parse-pred #'(lambda (c) (not (is-delimiter c))))) #'(lambda (x) (list (intern (string-upcase (implode x)))))))

(defun parse-string-char ()
  #'(lambda (stream)
      (let ((res (get-byte stream)))
        (if (null res)
            nil ; Тест на конец потока
            (let ((char (car res))
                  (next-stream (cdr res)))
              (if (eq char #\\)
                  (get-byte next-stream)
                  (if (eq char #\")
                      nil
                      res
                  )))))))

(defun parse-string-token ()
  (parse-app (&&& (parse-elem #\")
                  (parse-many (parse-string-char))
                  (parse-elem #\"))
             #'(lambda (x) (list (implode (cadr x))))))

(defun parse-comma ()
  (parse-app (&&& (parse-elem #\,) (parse-optional (parse-elem #\@)))
             #'(lambda (x) (if (cadr x) (list 'COMMA-AT) (list 'COMMA)))))

(defun parse-comma-token ()
    (parse-app (&&& (parse-comma) (parse-rec (parse-token)))
        #'(lambda (x) (append (car x) (cadr x)))))

(defun parse-char-sharp ()
  (parse-app (&&& (parse-elem #\\) (parse-pred #'(lambda (x) t)))
             #'(lambda (x) (list 'T-CHAR (cadr x)))))

(defun parse-function-sharp ()
  (parse-app (&&& (parse-elem #\') (parse-symbol-token))
             #'(lambda (x) (list 'FUNCTION (caadr x)))))

(defun parse-sharp ()
  (parse-app (&&& (parse-elem #\#)
                  (parse-or (parse-char-sharp)
                            (parse-function-sharp)
                            (parse-app (parse-rec (parse-token)) #'(lambda (x) (append (list 'SHARP) x)))))
             #'cadr))

(defun parse-quote-token ()
  (parse-app (&&& (parse-elem #\') (parse-rec (parse-token)))
             #'(lambda (x) (append (list 'QUOTE) (cadr x)))))

(defun parse-backquote-token ()
  (parse-app (&&& (parse-elem #\`) (parse-rec (parse-token)))
             #'(lambda (x) (append (list 'BACKQUOTE) (cadr x)))))

(defun parse-token ()
  (parse-or (parse-lparen)
            (parse-rparen)
            (parse-hex-number)
            (parse-float-token-2)
            (parse-number-token)
            (parse-float-token)
            (parse-string-token)
            (parse-sharp)
            (parse-quote-token)
            (parse-backquote-token)
            (parse-comma-token)
            (parse-dot)
            (parse-symbol-token)
            #'(lambda (stream) (throw 'parse-error "Unknown token"))))

(defun lexer (str)
  (labels ((lex-loop (s acc)
             (let ((s-no-ws (cdr (funcall (parse-many (parse-pred #'is-whitespace)) s))))
               (if (is-eoi-str s-no-ws)
                   acc
                   (let ((res (funcall (parse-token) s-no-ws)))
                     (if res
                         (lex-loop (cdr res) (append acc (car res)))
                        acc))))))
    (lex-loop (stream-from-str str) nil)))
