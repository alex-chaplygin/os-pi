;; Генерация кода после анализа компилятора

;; *program* - хранит накопленный результат компиляции.
(defvar *program*)

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
  (let ((l (gensym)))
    (emit (list 'JMP l))
    (emit (list (car expr) (cadr expr)))
    (inner-generate (caddr expr))
    (emit (list 'LABEL l))))

;; Генерация кода для присваиваний
(defun generate-set (expr)
  (inner-generate (caddr expr))
  (emit (list (car expr) (cadr expr))))

;; Генерация вычисления аргументов
(defun generate-args (args set)
  (let ((i 0))
    (dolist (a args)
      (inner-generate a) ; код аргумента
      (emit (if (equal set 'PUSH)
		(list set)
		(list set i)))
      (incf i))))

;; Вызов примитива
(defun generate-prim (type args)
  (generate-args args 'PUSH)
  (emit (list 'PRIM type)))

;; Обычный вызов функции
(defun generate-reg-call (name env args)
  (let ((num (list-length args)))
    (generate-args args 'PUSH)
    (emit (list 'SAVE-ENV))
    (emit (list 'SET-ENV env))
    (emit (list 'ALLOC num))
    (emit (list 'REG-CALL name))
    (emit (list 'RESTORE-ENV))))

;; let - форма, расширение окружения, тело lambda без вызова функции
(defun generate-let (count args body)
  (generate-args args 'PUSH)
  (emit (list 'SAVE-ENV))
  (emit (list 'ALLOC count))
  (inner-generate body)
  (emit (list 'RESTORE-ENV)))

;; генерация замыкания
(defun generate-closure (name body)
  (emit (list 'FIX-CLOSURE name))
  (when body
    (inner-generate body)))

;; Генерация кода
(defun inner-generate (expr)
  ;; (print (list 'inner-generate expr))
  (if (contains '(CONST GLOBAL-REF LOCAL-REF DEEP-REF RETURN) (car expr))
      (emit expr)
      (if (contains '(GLOBAL-SET LOCAL-SET DEEP-SET) (car expr))
	  (generate-set expr)
	  (if (eq 'LABEL (car expr))
	      (generate-2-params expr)
	      (case (car expr)
		('NOP nil)
		('SEQ (app #'inner-generate (cdr expr))) ; последовательность
		('ALTER (generate-if (cdr expr))) ; ветвление
		('FIX-LET (generate-let (cadr expr) (caddr expr) (cadddr expr))) ; let форма
		('FIX-CLOSURE (generate-closure (cadr expr) (caddr expr))) ; замыкание
		('PRIM (generate-prim (cadr expr) (caddr expr))) ; вызов примитива
		('REG-CALL (generate-reg-call (cadr expr) (caddr expr) (cadddr expr))) ; обычный вызов
		(otherwise (emit (list 'UNKNOWN (car expr)))))))))

(defun generate (expr)
  (setq *program* nil)
  (inner-generate expr)
  *program*)
