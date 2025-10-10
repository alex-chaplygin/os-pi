(defun parse-suc (val)
  "Элементарный парсер - успешный разбор со значением val"
  #'(lambda (list) (list (cons val list))))

(defun parse-fail ()
  "Элементарный парсер - неудачный разбор"
  #'(lambda (list) nil))

(defun parse-pred (pred)
  "Парсер по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t."
  #'(lambda (list)
      (if (or (null list) (not (funcall pred (car list))))
          nil
          (list (cons (car list) (cdr list))))))

(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент в списке"
  (parse-pred #'(lambda (x) (eq x sym))))

(defun &&& (&rest parsers)
  "Последовательный комбинатор"
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null parsers) (list (cons res list))
		     (mapcan #'(lambda (p)
				 (apply-parser (cdr parsers) (cdr p) (append res (list (car p)))))
			     (funcall (car parsers) list)))))
	(apply-parser parsers list nil))))

(defun parse-or (&rest parsers)
  "Параллельный комбинатор"
  #'(lambda (list)
      (mapcan #'(lambda (p) (funcall p list)) parsers)))
		     
(defun parse-app (parser f)
  "Комбинатор применения функции ко всем результатам разбора"
  #'(lambda (list)
      (let ((res (funcall parser list)))
	(mapcar #'(lambda (r) (cons (funcall f (car r)) (cdr r))) res))))

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений"
  (parse-or (parse-some parser) (parse-suc nil)))

(defun parse-some (parser)
  "Комбинатор - 1 или более повторений"
  (parse-app (&&& parser #'(lambda (list) (funcall (parse-many parser) list)))
	     #'(lambda (x) (cons (car x) (cadr x)))))

(defun parse-many-sep (parser sep)
  "Комбинатор - 0 или более повторений с разделителем"
  (parse-or (parse-some-sep parser sep) (parse-suc nil)))

(defun parse-some-sep (parser sep)
  "Комбинатор - 1 или более повторений с разделителем"
  (parse-app (&&& parser (parse-many (parse-app (&&& sep parser) #'cadr)))
	     #'(lambda (x) (cons (car x) (cadr x)))))

;; ---------------
;; Lisp parser
;; ---------------

(defun p-token-type (type)
  (parse-pred #'(lambda (token) (and token (eq (car token) type)))))

(defun p-number () (parse-app (p-token-type :T_NUMBER) #'cadr))
(defun p-float () (parse-app (p-token-type :T_FLOAT) #'cadr))
(defun p-string () (parse-app (p-token-type :T_STRING) #'cadr))
(defun p-char () (parse-app (p-token-type :T_CHAR) #'(lambda (tok) (code-char (cadr tok)))))
(defun p-symbol () (parse-app (p-token-type :T_SYMBOL) #'(lambda (tok) (intern (cadr tok)))))

(defun p-atom (input) (funcall (parse-or (p-number) (p-float) (p-string) (p-char) (p-symbol)) input))

(defun build-dotted-list (exprs last-expr)
  (if (null exprs)
      last-expr
      (cons (car exprs) (build-dotted-list (cdr exprs) last-expr))))

(defun p-list (input)
  (funcall (parse-app
   (&&& (p-token-type :LPAREN)
        (parse-or (parse-many-sep #'p-expr (p-token-type :COMMA)) (parse-many #'p-expr))
        (parse-or (parse-app (&&& (p-token-type :DOT) #'p-expr) #'cadr)
                  (parse-suc nil))
        (p-token-type :RPAREN))
   #'(lambda (res)
       (let ((exprs (cadr res))
             (dot-expr (caddr res)))
         (if dot-expr
             (build-dotted-list exprs dot-expr)
             exprs))))
           input))

(defun p-vector (input)
  (funcall (parse-app (&&& (p-token-type :SHARP)
                  (p-token-type :LPAREN)
                  (parse-many #'p-expr)
                  (p-token-type :RPAREN))
             #'(lambda (res) (coerce (caddr res) 'vector)))
           input))

(defun p-quote (input) (funcall (parse-app (&&& (p-token-type :QUOTE) #'p-expr) #'(lambda (res) `(QUOTE ,(cadr res)))) input))
(defun p-backquote (input) (funcall (parse-app (&&& (p-token-type :BACKQUOTE) #'p-expr) #'(lambda (res) `(BACKQUOTE ,(cadr res)))) input))
(defun p-comma (input) (funcall (parse-app (&&& (p-token-type :COMMA) #'p-expr) #'(lambda (res) `(COMMA ,(cadr res)))) input))
(defun p-comma-at (input) (funcall (parse-app (&&& (p-token-type :COMMA_AT) #'p-expr) #'(lambda (res) `(COMMA-AT ,(cadr res)))) input))
(defun p-function-quote (input) (funcall (parse-app (&&& (p-token-type :T_FUNCTION) #'p-expr) #'(lambda (res) `(FUNCTION ,(cadr res)))) input))

(defun p-expr (input)
  (funcall (parse-or #'p-atom
                     #'p-list
                     #'p-vector
                     #'p-quote
                     #'p-backquote
                     #'p-comma
                     #'p-comma-at
                     #'p-function-quote)
           input))

(defun parse (token-stream)
  ;; --- Запуск парсера ---
  (let* ((results (p-expr token-stream))
         (first-good-result (car results)))
    (cond
      ((null results)
       (error "Parse error: invalid syntax."))
      ((cdr first-good-result)
       (error "Parse error: unparsed tokens remaining: ~s" (cdr first-good-result)))
      (t
       (car first-good-result)))))