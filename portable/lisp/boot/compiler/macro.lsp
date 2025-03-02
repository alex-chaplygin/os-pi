;; глобальный список макросов (имя <число аргументов> аргументы тело)
(defvar *fix-macros*)
;; глобальный список nary макросов (имя <минимальное число аргументов> аргументы тело)
(defvar *nary-macros*)

;; добавить новый макрос
;; list (name args body)
(defun make-macro (list)
  (let ((name (car list))
	(args (second list))
	(body (cddr list)))
    (if (is-nary args)
	(add-nary-macro name 0 (num-fix-args args 0) (remove-rest args) body)
	(add-fix-macro name 0 (list-length args) args body))))

;; Сформировать правильный список аргументов
;; count - число постоянных аргументов
;; args - список аргументов
(defun make-nary-args (count args)
  (if (equal count 0) (list args)
      (cons (car args) (make-nary-args (-- count) (cdr args)))))

;; подстановка символа
(defun subst (sym env)
  (if (eq sym 'nil) nil
      (if (eq sym 't) t
  (let ((r (search-symbol env sym)))
    (if (null r) 
      (comp-err (concat "Unknown symbol in macro " (symbol-name sym)))
      (cdr r))))))

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

;; связвание переменных let
(defun macro-eval-let (args env)
  (when (< (list-length args) 1)
    (comp-err "macro-eval-let: invalid arguments count"))
  (let* ((decl (car args))
	 (body (cdr args))
	 (vars (get-vars decl))
	 (vals (macro-eval-args (get-vals decl) env)))
    (macro-eval-progn body (extend-macro-env env vars vals))))

;; вычисление аргументов
(defun macro-eval-args (args env)
  (map #'(lambda (a) (macro-eval a env)) args))

;; вычисление примитива
(defmacro eval-prim (expr)
  `(funcall #',(car expr) ,@(cdr expr)))

;; расширение окружения подстановки
(defun extend-macro-env (env args vals)
  (append (zip-with #'cons args vals) env))

;; применение пользовательской функции внутри макроса
(defun macro-eval-app-func (args body vals env)
  (macro-eval body (extend-macro-env env args vals)))

;; применение функции
(defun macro-eval-app (f args env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (count (second fun)))
    (check-arguments type count args)
    (let ((r
    (cond ((contains '(fix-prim nary-prim) type) (eval-prim `(,f ,@(map #'(lambda (a) (list 'quote a)) args))))
	  ((eq 'fix-func type) (macro-eval-app-func (forth fun) (fifth fun) args env))
	  (t (comp-err "macro-eval-app: invalid function" f)))))
      r)))

;; вычисление последовательности
(defun macro-eval-progn (expr env)
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (macro-eval-progn (cdr expr) env))))

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

;; раскрытие макроса
(defun macro-eval (expr env)
;  (print `(meval ,expr ,env))
  (if (atom expr)
      (if (symbolp expr) (subst expr env) expr)
      (let ((f (car expr))
	    (args (cdr expr)))
	(case f
	  ('if (macro-eval-if args env))
	  ('cond (macro-eval-cond args env))
	  ('progn (macro-eval-progn args env))
	  ('let (macro-eval-let args env))
	  ('quote (car args))
	  ('backquote (macro-eval-backquote (car args) env))
	  ('function expr)
	  ('funcall (macro-eval-funcall (macro-eval (car args) env) (macro-eval-args (cdr args) env) env))
	  (otherwise (macro-eval-app f (macro-eval-args args env) env))))))

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
      (print `(macro-expand args ,args vals ,vals body ,body res ,r))
      r)))
