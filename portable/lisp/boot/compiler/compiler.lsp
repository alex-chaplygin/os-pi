(defvar *program* nil)


;; (defun compile-func (func-body)
;;   ;(print `(compile-func ,func-body))
;;   (unless (null func-body)
;;     (let ((func (car func-body))
;;           (args (cdr func-body)))
;;       (cond
;;         ((eq func 'progn)
;;          (while (not (null args))
;; 	   ;(print args)
;;            (inner-compile `(,(car args)))
;;            (setq args (cdr args))))
;;         (t nil)))))

;; (defun inner-compile (expr)
;;   ;(print `(inner-compile ,expr-list))
;;   ;(print expr-list)
;;   ;(print *program*)
;;   (unless (null expr)
;;     (let ((expr (car expr-list)))
;;       (if (atom expr)
;; 	  (list-add *program* `((lda ,expr)))
;; 	(compile-func expr))
;;       ;(print expr-list)
;;       ;(unless (null expr-list)
;;       ;(print *program*)
;;       (inner-compile (cdr expr-list))))))

(defun emit (val)
;  (print `(emit ,*program* ,val))
  (setq *program* (append *program* val)))

(defun compile-progn (lst)
 ; (print `(compile-progn ,lst))
  (cond ((eq lst nil) nil)
	(t
	 (progn
	   (inner-compile (car lst))
	   (compile-progn (cdr lst))))))

(defun inner-compile (expr)
  ;(print `(inner-compile ,expr))
  (cond ((atom expr) (emit `((lda ,expr))))
	(t
;    (progn
;      (print `(cdr ,expr))
	(cond ((eq (car expr) 'progn) (compile-progn (cdr expr)))
	      (t nil)))))

(defun compile (expr)
  (setq *program* nil)
  (inner-compile expr)
  (when (null *program*)
    (setq *program* '((lda nil))))
  *program*)
