;; список примитивов с фиксированным количеством аргументов
(defvar *fix-primitives*
  '((car . 1) (cdr . 1) (atom . 1) (cons . 2) (rplaca . 2) (rplacd . 2)
    (% . 2) (<< . 2) (>> . 2) (eq . 2) (equal . 2) (> . 2) (< . 2) (sin . 1) (cos . 1) (sqrt . 1)
    (intern . 1) (symbol-name . 1) (string-size . 1) (inttostr . 1) (code-char . 1) (char-code . 1) (putchar . 1) (char . 2) (subseq . 3)
    (make-array . 1) (make-string . 2) (array-size . 1) (aref . 2) (seta . 3) (sets . 3)
    (symbolp . 1) (integerp . 1) (pairp . 1) (functionp . 1) (gensym . 0)))
;; список примитивов с переменным количеством аргументов
(defvar *nary-primitives*
  '((+ . 0) (- . 1) (* . 0) (/ . 1) (& . 0) (bitor . 0) (^ . 0) (concat . 0) (funcall . 1) (print . 0) (error . 0)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg &rest other)
  (throw 'compiler (cons msg other)))

(defun is-nary (args) ;; переменное число аргументов?
  (contains args '&rest))

;; Добавить глобальную функцию с именем, смещением окружения и числом аргументов
(defmacro mk/add-func (name list &rest other)
  `(defun ,name (name env arity ,@other)
     (setq ,list (cons (list name env arity ,@other) ,list))))

(defun num-fix-args (list num) ;; определить число фиксированных аргументов
  (if (eq (car list) '&rest) num (num-fix-args (cdr list) (++ num))))

(defun remove-rest (list) ;; удалить &rest из списка аргументов
  (if (null list) nil
      (if (eq (car list) '&rest) (cdr list)
	  (cons (car list) (remove-rest (cdr list))))))

;; Сформировать правильный список аргументов
;; count - число постоянных аргументов
;; args - список аргументов
(defun make-nary-args (count args)
  (if (equal count 0) (list args)
      (cons (car args) (make-nary-args (-- count) (cdr args)))))

;; Поиск функции или примитива по имени, возвращет сохраненную функцию или примитив
(defun search-symbol (list name)
  (labels ((search (list)
	     (if (null list) nil
		 (if (eq (caar list) name) (car list)
		     (search (cdr list))))))
    (search list)))
