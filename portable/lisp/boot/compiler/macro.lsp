;; глобальный список макросов (имя <число аргументов> аргументы тело)
(defvar *fix-macros*)
;; глобальный список nary макросов (имя <минимальное число аргументов> аргументы тело)
(defvar *nary-macros*)
;; глобальные переменные макро мира
(defvar *macro-globals* (make-hash))

(mk/add-func add-fix-macro *fix-macros* args body) ;; макросы - фиксированное число аргументов
(mk/add-func add-nary-macro *nary-macros* args body) ;; макросы - переменное число аргументов

;; добавить новый макрос
;; list (name args body)
(defun make-macro (list)
  (let ((name (car list))
	(args (second list))
	(body (cddr list)))
    (if (is-nary args)
	(add-nary-macro name 0 (num-fix-args args 0) (remove-rest args) body)
	(add-fix-macro name 0 (list-length args) args body))))

;; подстановка символа или значения переменной из глобального состояния
(defun subst (sym env)
  (if (eq sym 'nil) nil
      (if (eq sym 't) t
	  (let ((r (search-symbol env sym)))
	   ;; (print `(subst ,sym ,r ,env))
    (if (null r)
	(if (check-key *macro-globals* sym) (get-hash *macro-globals* sym)
	    (comp-err "Unknown symbol in macro "  sym env))
	(cdr r))))))

(defun macro-eval (expr env) nil)

;; квазицитирование
(defun macro-eval-backquote (expr env)
  (if (null expr) nil
      (if (atom expr) expr
	  (cond ((eq (car expr) 'comma) (macro-eval (second expr) env))
		((and (pairp (car expr)) (eq (caar expr) 'comma-at))
		 (append (macro-eval (cadar expr) env) (macro-eval-backquote (cdr expr) env)))
		(t
		 (cons (macro-eval-backquote (car expr) env) (macro-eval-backquote (cdr expr) env)))))))

; условие
(defun macro-eval-if (expr env)
  (when (!= (list-length expr) 3)
    (comp-err "macro-eval-if: invalid arguments count"))
  (let ((cond (car expr))
	(true (second expr))
	(false (third expr)))
    (if (null (macro-eval cond env)) (macro-eval false env) (macro-eval true env))))

;; вычисление аргументов
(defun macro-eval-args (args env)
  (map #'(lambda (a) (macro-eval a env)) args))

;; вычисление последовательности
(defun macro-eval-progn (expr env)
;;  (print `(eval-progn ,expr))
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (progn
	    (macro-eval (car expr) env)
	    (macro-eval-progn (cdr expr) env)))))

;; расширение окружения подстановки
(defun extend-macro-env (env args vals)
  (append (zip-with #'cons args vals) env))

;; связвание переменных let
(defun macro-eval-let (args env)
  (when (< (list-length args) 1)
    (comp-err "macro-eval-let: invalid arguments count"))
  (let* ((decl (car args))
	 (body (cdr args))
	 (vars (get-vars decl))
	 (vals (macro-eval-args (get-vals decl) env)))
    (macro-eval-progn body (extend-macro-env env vars vals))))

;; генерация функции вычисления примитива
(defmacro gen-eval-prim (prims)
  `(defun eval-prim (f args)
     (apply (case f ,@(map #'(lambda (pr) `(',(car pr) #',(car pr))) ,prims)) args)))

(gen-eval-prim (append *fix-primitives* *nary-primitives*))

;; применение пользовательской функции внутри макроса
(defun macro-eval-app-func (args body vals env)
  (macro-eval body (extend-macro-env env args vals)))

(defun find-func (f)
  nil)

;; Проверка числа аргументов
(defun check-arguments (f type count args)
    (when (contains '(lambda fix-func local-func fix-prim fix-macro) type)
      (when (!= count (list-length args))
	(comp-err "invalid args count" f count args)))
    (when (contains '(nary-prim nary-func nary-macro) type)
      (when (> count (list-length args))
	(comp-err "invalid nary args count" f count args))))

;; применение функции
(defun macro-eval-app (f args env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (count (second fun)))
    ;;(print `(eval-app ,f ,args ,env ,fun))
    (check-arguments f type count args)
    (let ((r
	    (cond ((contains '(fix-prim nary-prim) type) (eval-prim f (macro-eval-args args env)))
	  ((eq 'fix-func type) (macro-eval-app-func (forth fun) (fifth fun) (macro-eval-args args env) env))
	  ((eq 'nary-func type) (macro-eval-app-func (remove-rest (forth fun)) (fifth fun) (make-nary-args count (macro-eval-args args env)) env))
	  ((eq 'fix-macro type) (macro-eval (macroexpand (third fun) args (forth fun)) env))
	  ((eq 'nary-macro type) (macro-eval (macroexpand (third fun) (make-nary-args count args) (forth fun)) env))
	  (t (comp-err "macro-eval-app: invalid function" f type)))))
      r)))

;; вычисление вызова функции
;; подставить тело lambda, расширить окружение подстановки
(defun macro-eval-funcall (f args env)
  (let ((type (car f))
	(lam (second f)))
    (if (and (eq type 'function) (correct-lambda lam))
      (macro-eval-progn (cddr lam) (extend-macro-env env (second lam) args))
      (comp-err "macro-eval-funcall: invalid function" f))))

;; вычисление cond
(defun macro-eval-cond (list env)
  (if (null list) nil
      (let ((e (car list)))
	(when (or (not (pairp e)) (null e) (null (cdr e)) (not (null (cddr e))))
	  (comp-err "macro-eval: invalid expression in cond" e))
	(if (not (eq (macro-eval (car e) env) nil)) (macro-eval (second e) env)
	    (macro-eval-cond (cdr list) env)))))

;; присвоение глобальной переменной в мире макросов
(defun macro-eval-setq (var expr env)
  (unless (symbolp var)
    (comp-err "macro setq: not variable" var))
  (let ((val (macro-eval expr env)))
    (set-hash *macro-globals* var val)))

;; раскрытие макроса
(defun macro-eval (expr env)
  ;;(print `(meval ,expr ,env))
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
	  ('function expr)
	  ('funcall (macro-eval-funcall (macro-eval (car args) env) (macro-eval-args (cdr args) env) env))
	  (otherwise (macro-eval-app f args env))))))

;; раскрытие последовательности
(defun macro-expand-progn (expr env)
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (list 'progn (macro-eval (car expr) env) (macro-expand-progn (cdr expr) env)))))

;; раскрытие макроса, args - аргументы макроса
;; vals - подстановочные значения параметров
;; body - тело макроса
(defun macroexpand (args vals body)
  (labels ((make (args vals)
	     (if (null args) nil
		 (if (eq (car args) '&rest)
		     (list (cons (second args) vals))
		     (cons (cons (car args) (car vals)) (make (cdr args) (cdr vals)))))))
    (let ((r (macro-expand-progn body (make args vals))))
;;      (print `(macro-expand ,r))
      r)))
