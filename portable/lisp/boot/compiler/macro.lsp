;; глобальный список макросов (имя <число аргументов> аргументы тело)
(defvar *fix-macros*)
;; глобальный список nary макросов (имя <минимальное число аргументов> аргументы тело)
(defvar *nary-macros*)
;; глобальные переменные макро мира
(defvar *macro-globals* (make-hash))

(mk/add-func add-fix-macro *fix-macros* args body) ;; макросы - фиксированное число аргументов
(mk/add-func add-nary-macro *nary-macros* args body) ;; макросы - переменное число аргументов

(defun make-macro (list)
";; добавить новый макрос"
;; list (name args body)
  (let ((name (car list))
	(args (second list))
	(body (cddr list)))
    (if (is-nary args)
	(add-nary-macro name 0 (num-fix-args args 0) (remove-rest args) body)
	(add-fix-macro name 0 (list-length args) args body))))

(defun subst (sym env)
";; подстановка символа или значения переменной из глобального состояния"
  (if (eq sym 'nil) nil
      (if (eq sym 't) t
	  (let ((r (search-symbol env sym)))
	   ;; (print `(subst ,sym ,r ,env))
    (if (null r)
	(if (check-key *macro-globals* sym) (get-hash *macro-globals* sym)
	    (comp-err "Unknown symbol in macro "  sym env))
	(cdr r))))))

(defun macro-eval (expr env) nil)

(defun macro-eval-backquote (expr env)
"макровычисление квазицитирования"
  ;;(print `(meval-backquote ,expr ,env))
  (if (null expr) nil
      (if (or (atom expr) (eq (car expr) 'backquote)) expr
	  (let ((el (car expr)))
	    (cond ((eq el 'comma) (macro-eval (second expr) env))
		  ((and (pairp el) (not (null el)) (eq (caar expr) 'comma-at))
		 (append (macro-eval (cadar expr) env) (macro-eval-backquote (cdr expr) env)))
		(t
		 (cons (macro-eval-backquote (car expr) env) (macro-eval-backquote (cdr expr) env))))))))

(defun macro-eval-if (expr env)
"макровычисление условия"
  (when (!= (list-length expr) 3)
    (comp-err "macro-eval-if: invalid arguments count"))
  (let ((cond (car expr))
	(true (second expr))
	(false (third expr)))
    (if (null (macro-eval cond env)) (macro-eval false env) (macro-eval true env))))

(defun macro-eval-args (args env)
"макровычисление аргументов"
  (map #'(lambda (a) (macro-eval a env)) args))

(defun macro-eval-progn (expr env)
"макровычисление последовательности"
;;  (print `(eval-progn ,expr))
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (progn
	    (macro-eval (car expr) env)
	    (macro-eval-progn (cdr expr) env)))))

(defun extend-macro-env (env args vals)
"макро расширение окружения подстановки"
  (append (zip-with #'cons args vals) env))

(defun macro-eval-let (args env)
"макро связвание переменных let"
  (when (< (list-length args) 1)
    (comp-err "macro-eval-let: invalid arguments count"))
  (let* ((decl (car args))
	 (body (cdr args))
	 (vars (get-vars decl))
	 (vals (macro-eval-args (get-vals decl) env)))
    (macro-eval-progn body (extend-macro-env env vars vals))))

(defun macro-eval-app-func (args body vals env)
";; применение пользовательской функции внутри макроса"
  (macro-eval body (extend-macro-env env args vals)))

(defun find-func (f)
  nil)

(defun check-arguments (f type count args)
";; Проверка числа аргументов"
    (when (contains '(lambda fix-func local-func fix-prim fix-macro) type)
      (when (!= count (list-length args))
	(comp-err "invalid args count" f count args)))
    (when (contains '(nary-prim nary-func nary-macro) type)
      (when (> count (list-length args))
	(comp-err "invalid nary args count" f count args))))

(defun macroexpand (args vals body) nil)

(defun macro-eval-app (f args env)
"макро применение функции"
  (let* ((fun (find-func f))
	 (type (car fun))
	 (count (second fun)))
;;    (print `(eval-app ,f ,args ,env ,fun))
    (check-arguments f type count args)
    (let ((r
	    (cond ((contains '(fix-prim nary-prim) type) (apply (symbol-function f) (macro-eval-args args env)))
	  ((eq 'fix-func type) (macro-eval-app-func (forth fun) (fifth fun) (macro-eval-args args env) env))
	  ((eq 'nary-func type) (macro-eval-app-func (remove-rest (forth fun)) (fifth fun) (make-nary-args count (macro-eval-args args env)) env))
	  ((eq 'fix-macro type) (macro-eval (macroexpand (third fun) args (forth fun)) env))
	  ((eq 'nary-macro type) (macro-eval (macroexpand (third fun) (make-nary-args count args) (forth fun)) env))
	  (t (comp-err "macro-eval-app: invalid function" f type)))))
      r)))

(defun macro-eval-funcall (f args env)
"макро вычисление вызова функции"
;; подставить тело lambda, расширить окружение подстановки
  (let ((type (car f))
  	(lam (second f)))
    (if (and (eq type 'function) (correct-lambda lam))
      (macro-eval-progn (cddr lam) (extend-macro-env env (second lam) args))
      (comp-err "macro-eval-funcall: invalid function" f))))

(defun macro-eval-cond (list env)
"макро вычисление cond"
  (if (null list) nil
      (let ((e (car list)))
	(when (or (not (pairp e)) (null e) (null (cdr e)) (not (null (cddr e))))
	  (comp-err "macro-eval: invalid expression in cond" e))
	(if (not (eq (macro-eval (car e) env) nil)) (macro-eval (second e) env)
	    (macro-eval-cond (cdr list) env)))))

(defun macro-eval-setq (var expr env)
";; присвоение глобальной переменной в мире макросов"
  (unless (symbolp var)
    (comp-err "macro setq: not variable" var))
  (let ((val (macro-eval expr env)))
    (set-hash *macro-globals* var val)))

(defun macro-eval-function (s)
"макро вычисление объекта-функции"
  (if (symbolp s) (symbol-function s)
      (if (correct-lambda s) (list 'function s)
	  (error "macro-eval-function: invalid function" s))))
   
(defun macro-eval (expr env)
"рекурсивное раскрытие макроса"
  ;;(print `(meval ,expr ,env))
  (if (functionp expr) expr
  (if (atom expr)
      (if (symbolp expr) (subst expr env) expr)
      (let ((f (car expr))
	    (args (cdr expr)))
	(case f
	  ('if (macro-eval-if args env))
	  ('cond (macro-eval-cond args env))
	  ('progn (macro-eval-progn args env))
	  ('setq (macro-eval-setq (car args) (second args) env))
	  ('let (macro-eval-let args env))
	  ('quote (car args))
	  ('backquote (macro-eval-backquote (car args) env))
	  ('function (macro-eval-function (second expr)))
	  ('funcall (macro-eval-funcall (macro-eval (car args) env) (macro-eval-args (cdr args) env) env))
	  (otherwise (macro-eval-app f args env)))))))

(defun macro-expand-progn (expr env)
"макро раскрытие последовательности"
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (list 'progn (macro-eval (car expr) env) (macro-expand-progn (cdr expr) env)))))

(defun macroexpand (args vals body)
";; раскрытие макроса, args - аргументы макроса"
;; vals - подстановочные значения параметров
;; body - тело макроса
  (labels ((make (args vals)
	     (if (null args) nil
		 (if (eq (car args) '&rest)
		     (list (cons (second args) vals))
		     (cons (cons (car args) (car vals)) (make (cdr args) (cdr vals)))))))
    ;;(print `(macro expand ,args ,vals ,body))
    (let ((r (macro-expand-progn body (make args vals))))
;;      (print `(macro-expand-res ,r))
      r)))
