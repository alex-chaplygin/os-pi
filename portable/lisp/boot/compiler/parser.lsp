; переменная для сбора ошибок
(defvar *syntax-analyze-errors* nil)

(defun signal-parse-error (message)
  "Регистрирует ошибку синтаксического анализа."
  (setq *syntax-analyze-errors* (cons message *syntax-analyze-errors*))
  nil)

(defun is-string (obj)
  "Проверяет, является ли объект строкой, перехватывая ошибку от string-size."
  (not (eq 'ERROR (catch 'ERROR (string-size obj)))))

(defun parse-atom (tokens)
  "Если токен - атом, то возвращает пару (токен . остальные_токены)"
  (let ((token (car tokens)))
    (if (or (symbolp token)
            (integerp token)
            (is-string token))
        (cons token (cdr tokens))
        nil)))

(defun parse-list (tokens)
  "Разбирает список токенов, начиная с '('.
   Возвращает (синтаксическое_дерево . остальные_токены)"
  (if (not (eq (car tokens) (code-char 40)))
      nil
      (labels ((parse-elements (toks acc)
                 (if (null toks)
                     (signal-parse-error "unclosed list")
                     (let ((head (car toks)))
                       (cond
                         ((eq head (code-char 41))
                          (cons (reverse acc) (cdr toks)))
                         ((eq head 'DOT)
                          (let* ((rest-toks (cdr toks))
                                 (last-expr-res (parse-s-expression rest-toks)))
                            (if (or (null last-expr-res) (not (eq (car (cdr last-expr-res)) (code-char 41))))
                                (signal-parse-error "invalid dotted list")
                                (cons (list-to-dotted-pair (reverse acc) (car last-expr-res))
                                      (cdr (cdr last-expr-res))))))
                         (t
                          (let ((res (parse-s-expression toks)))
			    (if res
				(parse-elements (cdr res) (cons (car res) acc))
				(signal-parse-error "invalid expression in list")))))))))
        (parse-elements (cdr tokens) nil))))

(defun list-to-dotted-pair (lst last-cdr)
  (if (null lst)
      last-cdr
      (cons (car lst) (list-to-dotted-pair (cdr lst) last-cdr))))

(defun parse-prefixed (prefix tokens)
  "Разбирает префиксные формы ('form, `form, ,form, ,@form, #'form)"
  (if (not (eq (car tokens) prefix))
      nil
    (let ((res (parse-s-expression (cdr tokens))))
      (if res      
          (cons (list prefix (car res)) (cdr res))
          (signal-parse-error "prefixed form expects an expression")))))

(defun parse-quote (tokens)
  "Разбирает форму 'expr"
  (parse-prefixed 'QUOTE tokens))

(defun parse-backquote (tokens)
  "Разбирает форму `expr"
  (parse-prefixed 'BACKQUOTE tokens))

(defun parser-parse-comma (tokens)
  "Разбирает форму ,expr"
  (parse-prefixed 'COMMA tokens))

(defun parse-comma-at (tokens)
  "Разбирает форму ,@expr"
  (parse-prefixed 'COMMA-AT tokens))

(defun parse-function (tokens)
  "Разбирает форму #'expr"
  (parse-prefixed 'FUNCTION tokens))

(defun parse-array (tokens)
  "Разбирает массив #(...) "
  (if (not (eq (car tokens) 'SHARP))
      nil
      (let ((res (parse-list (cdr tokens))))
	(if res
            (cons (cons 'make-array (car res)) (cdr res))
	    (signal-parse-error "invalid array body")))))

(defun parse-s-expression (tokens)
  "Разбирает одно S-выражение из списка токенов."
  (if (null tokens)
      nil
      (let ((token (car tokens)))
        (cond
          ((eq token (code-char 39)) (signal-parse-error "prefixed form expects an expression"))
          ((eq token (code-char 40)) (parse-list tokens))
          ((eq token (code-char 41)) (signal-parse-error "unexpected token"))
          ((eq token 'QUOTE) (parse-quote tokens))
          ((eq token 'BACKQUOTE) (parse-backquote tokens))
          ((eq token 'COMMA) (parser-parse-comma tokens))
          ((eq token 'COMMA-AT) (parse-comma-at tokens))
          ((eq token 'FUNCTION) (parse-function tokens))
          ((eq token 'SHARP) (parse-array tokens))
          ((eq token 'T-CHAR) (cons (cadr tokens) (cddr tokens)))
          (t (parse-atom tokens))))))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп и вернуть синтаксическое дерево."
  (setq *syntax-analyze-errors* nil)
  (let ((tokens (lexer str)))
    (if tokens
        (let ((res (parse-s-expression tokens)))
          (cond
            ((or (null res) *syntax-analyze-errors*) 'SYNTAX-ANALYZE-ERROR)
            ((not (null (cdr res)))
             (signal-parse-error "extra tokens at end of input")
             'SYNTAX-ANALYZE-ERROR)
            (t (car res))))
        nil)))
