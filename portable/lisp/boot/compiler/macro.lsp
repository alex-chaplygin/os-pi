;; глобальный список макросов (имя <число аргументов> аргументы тело)
(defvar *fix-macros*)
;; глобальный список nary макросов (имя <минимальное число аргументов> аргументы тело)
(defvar *nary-macros*)

;; добавить новый макрос
;; list (name args body)
(defun make-macro (list)
  (let ((name (car list))
	(args (cadr list))
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
  (let ((r (search-symbol env sym)))
    (if (null r) 
      (comp-err (concat "Unknown symbol in macro " (symbol-name sym)))
      (cdr r)))))

;; квазицитирование
(defun macro-eval-backquote (expr env)
  (if (null expr) nil
      (if (atom expr) expr
	  (cond ((eq (car expr) 'comma) (macro-eval (cadr expr) env))
		((and (pairp (car expr)) (eq (caar expr) 'comma-at))
		 (append (macro-eval (cadar expr) env) (macro-eval-backquote (cdr expr) env)))
		(t
		 (cons (macro-eval-backquote (car expr) env) (macro-eval-backquote (cdr expr) env)))))))

;; раскрытие макроса
(defun macro-eval (expr env)
  (if (atom expr)
      (if (symbolp expr) (subst expr env) expr)
      (case (car expr)
	('quote (cadr expr))
	('backquote (macro-eval-backquote (cadr expr) env)))))

;; раскрытие последовательности
(defun macro-eval-progn (expr env)
  (if (null expr) nil
      (if (null (cdr expr)) (macro-eval (car expr) env)
	  (list 'progn (macro-eval (car expr) env) (macro-eval-progn (cdr expr) env)))))

;; раскрытие макроса, args - аргументы макроса
;; vals - подстановочные значения параметров
;; body - тело макроса
(defun macroexpand (args vals body)
  (labels ((make (args vals)
	     (if (null args) nil
		 (if (eq (car args) '&rest)
		     (list (cons (cadr args) vals))
		     (cons (cons (car args) (car vals)) (make (cdr args) (cdr vals)))))))
    (macro-eval-progn body (make args vals))))
