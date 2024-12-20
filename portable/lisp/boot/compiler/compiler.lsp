;; *global-variables* - список имён глобальных переменных.
(defvar *global-variables*)
;; *global-variables-count* - число глобальных переменных в списке *global-variables*.
(defvar *global-variables-count*)
;; *global-functions* - глобальное окружение функций, содержащий названия функций, смещение кадра окружения, кол-во аргументов.
(defvar *global-functions*)
;; постоянный список примитивов с количеством их аргументов
(defvar *primitives*
  '((car . 1) (cdr . 1) (cons . 2) (rplaca . 2) (rplacd . 2)
    (+ . 2) (- . 2) (* . 2) (/ . 2) (% . 2) (& . 2) (bitor . 2) (<< . 2) (>> . 2) (equal . 2) (> . 2) (< . 2) (sin . 1) (cos . 1)
    (intern . 1) (symbol-name . 1) (string-size . 1) (inttostr . 1) (code-char . 1) (print . 1) (putchar . 1) (char . 2) (concat . 2) (subseq . 3)
    (make-array . 1) (array-size . 1) (aref . 2) (seta . 3)
    (symbolp . 1) (integerp . 1)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg)
  (return-from 'compiler msg))

;; Расширить окружение новым кадром аргументов
(defun extend-env (env args)
  (cons args env))

;; Добавить глобальную функцию с именем, смещением окружения и числом аргументов
(defun add-func (name env arity)
  (setq *global-functions* (cons (list name env arity) *global-functions*)))

;; Поиск функции или примитива по имени, возвращет сохраненную функцию или примитив
(defun search-symbol (list name)
  (labels ((search (list)
	     (if (null list) nil
		 (if (eq (caar list) name) (car list)
		     (search (cdr list))))))
    (search list)))

;; Проверка на правильность lambda выражения, или ошибка
(defun correct-lambda (f)
  (and (not (atom f))
       (>= (list-length f) 3)
       (equal (car f) 'lambda)
       (or (null (cadr f))
           (not (atom (cadr f))))
       (labels ((is-args-sym (args)
                  (if (null args) t
                      (if (symbolp (car args))
                          (is-args-sym (cdr args))
                          nil))))
         (is-args-sym (cadr f)))))

;; Комплирует лямбда-абстракцию.
;; (список аргументов, тело функции, локальное окружение).
(defun compile-lambda (name args body env)
  (let ((arity (list-length args)))
    (add-func name (list-length env) arity)
    (list 'LABEL name
	  (list 'SEQ
		(inner-compile body (extend-env env args))
		(list 'RETURN)))))

;; Определения типа функции: лямбда, примитив, глобальная функция
(defun find-func (f)
  (if (and (not (atom f)) (correct-lambda f))
      (list 'lambda (list-length (cadr f)) (gensym)) ; lambda num-args name
      (let ((r (search-symbol *macros* f)))
	(if r (list 'macro (list-length (cadr r)) (cadr r) (cddr r)) ; macro num-args args body
	    (let ((r (search-symbol *global-functions* f)))
	      (if r (list 'func (caddr r) (cadr r))  ; func num-args env
		  (let ((r (search-symbol *primitives* f)))
		    (if r (list 'primitive (cdr r)) ; primitive num-atrgs
			(comp-err (concat "unknown function " (symbol-name f)))))))))))

;; Применение функции
;; f - имя функции или lambda, args - аргументы, env - окружение
(defun compile-application (f args env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (count (cadr fun))
	 (cur-env (list-length env)))
    (if (!= count (list-length args))
	(comp-err (concat "invalid args count"))
	(if (eq type 'macro)
	    (inner-compile (macroexpand (caddr fun) args (cadddr fun)) env)
	    (let ((vals (map #'(lambda (a) (inner-compile a env)) args)))
	      (case type
		('lambda (list 'FIX-LET count vals (compile-progn (cddr f) (extend-env env (cadr f)))))
		('func (list 'REG-CALL f (caddr fun) vals))
		('primitive (list 'PRIM f vals))))))))

;; Создание функции-замыкания
(defun compile-function (f env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (name (gensym)))
    (case type
      ('lambda (list 'FIX-CLOSURE name (compile-lambda name (cadr f) `(progn ,@(cddr f)) env)))
      ('func (list 'FIX-CLOSURE f nil))
      (otherwise (comp-err (concat "invalid function argument"))))))

;; Компилирует объявление функции с помощью DEFUN.
;; expr - список, состоящий из названия, списка аргументов и тела функции.
(defun compile-defun (expr env)
  ;; проверка на правильность expr
  (let ((name (car expr))
	(args (cadr expr))
	(body (cddr expr)))
    (compile-lambda name args `(progn ,@body) env)))

;; Добавить глобальную переменную
;; Возвращает индекс добавленной переменной
(defun add-global (sym)
  (setq *global-variables* (append *global-variables* (list sym)))
  (incf *global-variables-count*)
  (- *global-variables-count* 1))

;; Компилирует setq-выражение.
;; setq-body - пара из символа и выражения, которое необходимо установить этому символу.
(defun compile-setq (body env)
  (if (null body)
      (comp-err "setq: no params")
      (labels ((compile-all-setq (body)
		 (if (= (list-length body) 2)
		     (compile-single-setq body)
		     (list 'SEQ (compile-single-setq body) (compile-all-setq (cddr body)))))
	       (compile-single-setq (body)
		 (if (< (list-length body) 2)
		     (comp-err "setq: no expression to set")
		     (let* ((var (car body))
			    (val (inner-compile (cadr body) env))
			    (res (find-var var env)))
		       (if (not (symbolp var))
			   (comp-err (concat "setq: invalid variable: " (symbol-name setq-sym)))
			   (if (null res)
			       (list 'GLOBAL-SET (add-global var) val)
			       (case (car res)
				 ('global (list 'GLOBAL-SET (cadr res) val))
				 ('local (list 'LOCAL-SET (cadr res) val))
				 ('deep (list 'DEEP-SET (cadr res) (caddr res) val))
				 (otherwise (error "Unreachable")))))))))
	    (compile-all-setq body))))

;; Компилирует if-выражение.
;; if-body - тело if-выражения (условие, ветка по "Да" и по "Нет").
(defun compile-if (expr env)
  (if (!= (list-length expr) 3)
      (comp-err "if: invalid params")  
      (let ((cond (inner-compile (car expr) env))
	    (true (inner-compile (cadr expr) env))
	    (false (inner-compile (caddr expr) env)))
      (list 'ALTER cond true false))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst env)
  (if (null lst) (compile-constant '())
    (if (null (cdr lst))
	(inner-compile (car lst) env)
      (list 'SEQ (inner-compile (car lst) env) (compile-progn (cdr lst) env)))))

;; Производит поиск переменной по символу сначала в локальном, а затем в глобальном окружении.
;; Возвращает список, состоящий из символа - GLOBAL или LOCAL, DEEP,
;; env - локальное окружение
(defun find-var (var env)
  (let ((local (find-local-var var env)))
    (if (not (null local)) local
        (find-global-var var))))

;; Производит поиск переменной в глобальном окружении по символу.
;; Возвращает индекс в массиве глобального окружения, если символ найден, иначе nil.
(defun find-global-var (var)
  (let ((i (list-search *global-variables* var)))
    (if (null i) nil
	(list 'global i))))

;; Производит поиск переменной в локальном окружении по символу.
;; Возвращает индекс переменной в стеке, если символ найден, иначе nil.
(defun find-local-var (var env)
  (labels ((find-frame (frames i)
	     (if (null frames) nil
		 (let ((j (list-search (car frames) var)))
		   (if (null j) (find-frame (cdr frames) (++ i))
		       (if (= i 0) (list 'local j)
			   (list 'deep i j)))))))
	  (find-frame env 0)))

;; Компиляция перемнной
(defun compile-variable (v env)
  (let ((res (find-var v env)))
    (if (null res)
	(comp-err (concat "Unknown symbol " (symbol-name v)))
      (case (car res)
	    ('local (list 'LOCAL-REF (cadr res)))
	    ('global (list 'GLOBAL-REF (cadr res)))
	    ('deep (list 'DEEP-REF (cadr res) (caddr res)))))))

;; Компиляция константы
(defun compile-constant (c)
  ;; (print (list 'compile-constant c))
  (list 'CONST c))

;; Комиляция квазицитирования
(defun compile-backquote (expr env)
  ;; (print (list 'compile-backquote expr))
  ;; (when (null expr)
  ;;   (comp-err "backquote: no body"))
  (if (atom expr)
      (compile-constant expr)
      (if (equal (car expr) 'COMMA)
	  (inner-compile (cadr expr) env)
	(list 'PRIM 'CONS (list (compile-backquote (car expr) env) (compile-backquote (cdr expr) env))))))

;; Функция компиляции в промежуточную форму
;; expr - выражение, env - лексическое окружение
(defun inner-compile (expr env)
  ;; (print (list 'inner-compile expr))
  (if (atom expr)
      (if (symbolp expr)
	  (compile-variable expr env)
	  (compile-constant expr))
      (let ((func (car expr))
	    (args (cdr expr)))
	(case func
	  ('quote (compile-constant (cadr expr)))
	  ('progn (compile-progn args env))
	  ('if (compile-if args env))
	  ('setq (compile-setq args env))
	  ('defun (compile-defun args env))
	  ('function (compile-function (cadr expr) env))
	  ('defmacro (progn (make-macro args) (list 'NOP)))
	  ('backquote (compile-backquote (cadr expr) env))
	  (otherwise (compile-application func args env))))))

;; Анализ S-выражения и преобразование в эквивалентное выражение
;; из более простых операций
(defun compile (expr)
  (setq *global-variables* '(t nil)
        *global-variables-count* (list-length *global-variables*)
        *comp-err* nil
        *comp-err-msg* nil
        *global-functions* nil
        *environment* nil)
  (block compiler
    (inner-compile expr nil)))
