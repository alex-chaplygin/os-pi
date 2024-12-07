;; Генерация кода после анализа компилятора

;; *program* - хранит накопленный результат компиляции.
(defvar *program)

;; Добавить инструкцию в программу
(defun emit (val)
  (setq *program* (append *program* (list val))))

;; Ветвление (cond true false)
(defun generate-if (expr)
  (let ((label-false (gensym))
	(label-after (gensym)))
    (inner-generate (car expr)) ; условие
    (emit (list 'JNT label-false))
    (inner-generate (cadr expr)) ; код по истине
    (emit (list 'JMP label-after))
    (emit (list 'LABEL label-false))
    (inner-generate (caddr expr)) ; код по лжи
    (emit (list 'LABEL label-after))))

;; Генерация функций с 2-мя параметрами
(defun generate-2-params (expr)
  (emit (list (car expr) (cadr expr)))
  (inner-generate (caddr expr)))

;; Генерация функций вызова со списком параметров
(defmacro make-call (name set-f)
  `(defun ,name (expr)
     (let ((i 0))
       (dolist (a (caddr expr))
	 (inner-generate a) ; код аргумента
	 (emit (list ',set-f i))
	 (incf i)))
     (emit (list (car expr) (cadr expr)))))
(make-call generate-prim SET-ARG)
(make-call generate-reg-call LOCAL-SET)

;; Генерация кода
(defun inner-generate (expr)
  (if (contains '(CONST GLOBAL-REF GLOBAL-SET LOCAL-REF LOCAL-SET DEEP-REF DEEP-SET ALLOC RETURN) (car expr))
      (emit expr)
      (if (contains '(LABEL FIX-CLOSURE) (car expr))
	  (generate-2-params expr)
	  (case (car expr)
	    ('SEQ (app #'inner-generate (cdr expr))) ; последовательность
	    ('ALTER (generate-if (cdr expr))) ; ветвление
	    ('PRIM (generate-prim expr)) ; вызов примитива
	    ('REG-CALL (generate-reg-call expr)) ; обычный вызов
	    (otherwise (emit (list 'UNKNOWN (car expr))))))))

(defun generate (expr)
  (setq *program* nil)
  (inner-generate expr)
  *program*)
