;; глобальный список макросов (имя аргументы тело)
(defvar *macros*)

;; добавить новый макрос
(defun make-macro (args)
  (setq *macros* (cons args *macros*)))

;; подстановка символа
(defun subst (sym env)
  (let ((r (search-symbol env sym)))
    (if (null r) 
      (comp-err (concat "Unknown symbol in macro " (symbol-name sym)))
      (cdr r))))

;; квазицитирование
(defun macro-eval-backquote (expr env)
  (if (null expr) nil
      (if (atom expr) expr
	  (cond ((eq (car expr) 'comma) (macro-eval (cadr expr) env))
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
  (print `(macroexpand ,args ,vals ,body))
  (labels ((make (args vals)
	     (if (null args) nil
		 (cons (cons (car args) (car vals)) (make (cdr args) (cdr vals))))))
    (macro-eval-progn body (make args vals))))
