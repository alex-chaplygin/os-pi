;; *global-variables* - список имён глобальных переменных.
(defvar *global-variables*)
;; *global-variables-count* - число глобальных переменных в списке *global-variables*.
(defvar *global-variables-count*)
;; *fix-functions* - глобальное окружение функций, содержащий названия функций, смещение кадра окружения, кол-во аргументов.
(defvar *fix-functions*)
;; список функций с переменным числом аргументов
(defvar *nary-functions*)
;; список локальных функций (локальное имя, смещение кадра, кол-во аргументов, скомплированное имя)
(defvar *local-functions*)
;; список примитивов с фиксированным количеством аргументов
(defvar *fix-primitives*
  '((car . 1) (cdr . 1) (atom . 1) (cons . 2) (rplaca . 2) (rplacd . 2)
    (% . 2) (<< . 2) (>> . 2) (eq . 2) (equal . 2) (> . 2) (< . 2) (sin . 1) (cos . 1) (sqrt . 1)
    (intern . 1) (symbol-name . 1) (string-size . 1) (inttostr . 1) (code-char . 1) (char-code . 1) (putchar . 1) (char . 2) (subseq . 3)
    (make-array . 1) (make-string . 2) (array-size . 1) (aref . 2) (seta . 3) (sets . 3)
    (symbolp . 1) (integerp . 1) (pairp . 1) (functionp . 1) (gensym . 0)))
;; список примитивов с переменным количеством аргументов
(defvar *nary-primitives*
  '((+ . 0) (- . 1) (* . 0) (/ . 1) (& . 0) (bitor . 0) (concat . 0) (funcall . 1) (print . 0) (error . 0)))

;; Расширить окружение новым кадром аргументов
(defun extend-env (env args)
  (cons args env))

(mk/add-func add-func *fix-functions* args body) ;; фиксированное число аргументов
(mk/add-func add-nary-func *nary-functions* args body) ;; переменное число аргументов
(mk/add-func add-local-func *local-functions* real-name) ;; локальные функции

;; Проверка на правильность lambda выражения, или ошибка
(defun correct-lambda (f)
  (and (not (atom f))
       (>= (list-length f) 3)
       (equal (car f) 'lambda)
       (or (null (second f))
           (not (atom (second f))))
       (labels ((is-args-sym (args)
                  (if (null args) t
                      (if (symbolp (car args))
                          (is-args-sym (cdr args))
                          nil))))
         (is-args-sym (second f)))))

;; Компиляция тела функции
(defun compile-func-body (name args body env)
  (list 'LABEL name
	(list 'SEQ
	      (inner-compile body (extend-env env args))
	      (list 'RETURN))))

;; Комплирует лямбда-абстракцию.
;; (список аргументов, тело функции, локальное окружение).
(defun compile-lambda (name args body env)
    (if (is-nary args)
	(add-nary-func name (list-length env) (num-fix-args args 0) args body)
	(add-func name (list-length env) (list-length args) args body))
    (compile-func-body name args body env))

;; Определения типа функции: лямбда, примитив, nary примитив, глобальная функция, nary функция
(defun find-func (f)
  (if (and (not (atom f)) (correct-lambda f))
      (list 'lambda (list-length (second f)) (gensym)) ; lambda num-args name
      (let ((r nil))
	(cond
	  ((setq r (search-symbol *fix-macros* f))
	   (list 'fix-macro (third r) (forth r) (fifth r))) ; fix-macro num-args args body
	  ((setq r (search-symbol *nary-macros* f))
	   (list 'nary-macro (third r) (forth r) (fifth r))) ; nary-macro num-args args body
	  ((setq r (search-symbol *local-functions* f))
	   (list 'local-func (third r) (second r) (forth r)))  ; local-func num-args env real-name
	  ((setq r (search-symbol *fix-functions* f))
	   (list 'fix-func (third r) (second r) (forth r) (fifth r)))  ; fix-func num-args env args body
	  ((setq r (search-symbol *nary-functions* f))
	   (list 'nary-func (third r) (second r) (forth r) (fifth r)))  ; nary-func num-args env args body
	  ((setq r (search-symbol *fix-primitives* f))
	   (list 'fix-prim (cdr r))) ; fix-prim num-args
	  ((setq r (search-symbol *nary-primitives* f))
	   (list 'nary-prim (cdr r))) ; nary-prim num-args
	  (t (comp-err "unknown function " f))))))

;; Проверка числа аргументов
(defun check-arguments (f type count args)
    (when (contains '(lambda fix-func local-func fix-prim fix-macro) type)
      (when (!= count (list-length args))
	(comp-err "invalid args count" f count args)))
    (when (contains '(nary-prim nary-func nary-macro) type)
      (when (> count (list-length args))
	(comp-err "invalid nary args count" f count args))))

;; Применение функции
;; f - имя функции или lambda, args - аргументы, env - окружение
(defun compile-application (f args env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (count (second fun))
	 (cur-env (list-length env)))
    (check-arguments f type count args)
    (if (eq type 'fix-macro)
	(inner-compile (macroexpand (third fun) args (forth fun)) env)
	(if (eq type 'nary-macro)
	    (let ((r (macroexpand (third fun) (make-nary-args count args) (forth fun))))
	      (inner-compile r env))
	(let ((vals (map #'(lambda (a) (inner-compile a env)) args)))
	  (case type
	    ('lambda (list 'FIX-LET count vals (compile-progn (cddr f) (extend-env env (second f)))))
	    ('local-func (list 'FIX-CALL (forth fun) (third fun) vals))
	    ('fix-func (list 'FIX-CALL f (third fun) vals))
	    ('nary-func (list 'NARY-CALL f count (third fun) vals))
	    ('fix-prim (list 'FIX-PRIM f vals))
	    ('nary-prim (list 'NARY-PRIM f count vals))))))))

;; Создание функции-замыкания
(defun compile-function (f env)
  (let* ((fun (find-func f))
	 (type (car fun))
	 (name (gensym)))
    (case type
      ('lambda (list 'FIX-CLOSURE name (compile-lambda name (second f) (cons 'progn (cddr f)) env)))
      ('fix-func (list 'FIX-CLOSURE f nil))
      ('local-func (list 'FIX-CLOSURE (forth fun) nil))
      ('fix-prim (list 'PRIM-CLOSURE f))
      ('nary-prim (list 'NPRIM-CLOSURE f))
      (otherwise (comp-err "invalid function argument" f fun)))))

;; Компилирует объявление функции с помощью DEFUN.
;; expr - список, состоящий из названия, списка аргументов и тела функции.
(defun compile-defun (expr env)
  ;; проверка на правильность expr
  (let ((name (car expr))
	(args (second expr))
	(body (cddr expr)))
    (compile-lambda name args (cons 'progn body) env)))

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
			    (val (inner-compile (second body) env))
			    (res (find-var var env)))
		       (if (not (symbolp var))
			   (comp-err "setq: invalid variable: " (symbol-name setq-sym))
			   (if (null res)
			       (list 'GLOBAL-SET (add-global var) val)
			       (case (car res)
				 ('global (list 'GLOBAL-SET (second res) val))
				 ('local (list 'LOCAL-SET (second res) val))
				 ('deep (list 'DEEP-SET (second res) (third res) val))
				 (otherwise (error "Unreachable")))))))))
	    (compile-all-setq body))))

;; Компилирует if-выражение.
;; if-body - тело if-выражения (условие, ветка по "Да" и по "Нет").
(defun compile-if (expr env)
  (if (!= (list-length expr) 3)
      (comp-err "if: invalid params")  
      (let ((cond (inner-compile (car expr) env))
	    (true (inner-compile (second expr) env))
	    (false (inner-compile (third expr) env)))
      (list 'ALTER cond true false))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst env)
  (if (null lst) (compile-constant '())
    (if (null (cdr lst))
	(inner-compile (car lst) env)
      (cons 'SEQ (map #'(lambda (x) (inner-compile x env)) lst)))))

;; Компилирует labels.
;; lst - (<список функций> <форма1> ... <формаn>).
(defun compile-labels (lst env)
  (let ((old *local-functions*))
    (labels ((extend-func (funcs)
	       (app
		#'(lambda (f)
		    (add-local-func (car f) (list-length env) (list-length (second f)) (gensym))) funcs))
	     (compile-funcs (funcs)
	       (cons 'SEQ (map
			   #'(lambda (f)
			       (let ((name (forth (find-func (car f))))
				     (args (second f))
				     (body (cddr f)))
				 (compile-func-body name args (cons 'progn body) env)))
			   funcs))))
      (extend-func (car lst))
      (let ((f (compile-funcs (car lst)))
	    (body (compile-progn (cdr lst) env)))
	(setq *local-functions* old)
	(list 'SEQ f body)))))

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
	(comp-err "Unknown symbol" v)
      (case (car res)
	    ('local (list 'LOCAL-REF (second res)))
	    ('global (list 'GLOBAL-REF (second res)))
	    ('deep (list 'DEEP-REF (second res) (third res)))))))

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
	  (inner-compile (second expr) env)
	(list 'FIX-PRIM 'CONS (list (compile-backquote (car expr) env) (compile-backquote (cdr expr) env))))))

;; Компиляция tagbody
(defun compile-tagbody (body env)
  (if (null body) (list 'CONST 'nil)
      (list 'SEQ (let ((e (car body)))
		   (if (symbolp e) (list 'LABEL e)
		       (inner-compile e env)))
	    (compile-tagbody (cdr body) env))))

;; Компиляция CATCH
(defun compile-catch (args env)
     (list 'CATCH (inner-compile (car args) env) (compile-progn (cdr args) env)))

;; Компиляция THROW
(defun compile-throw (args env)
     (list 'THROW (inner-compile (car args) env) (inner-compile (second args) env)))

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
	  ('quote (compile-constant (second expr)))
	  ('progn (compile-progn args env))
	  ('if (compile-if args env))
	  ('setq (compile-setq args env))
	  ('defun (compile-defun args env))
	  ('labels (compile-labels args env))
	  ('function (compile-function (second expr) env))
	  ('defmacro (progn (make-macro args) (list 'NOP)))
	  ('backquote (compile-backquote (second expr) env))
	  ('tagbody (compile-tagbody (cdr expr) env))
	  ('go (list 'GOTO (second expr)))
	  ('catch (compile-catch (cdr expr) env))
	  ('throw (compile-throw (cdr expr) env))
	  (otherwise (compile-application func args env))))))

;; Анализ S-выражения и преобразование в эквивалентное выражение
;; из более простых операций
(defun compile (expr)
  (setq *global-variables* '(t nil)
        *global-variables-count* (list-length *global-variables*)
        *comp-err* nil
        *comp-err-msg* nil
        *fix-functions* nil
        *environment* nil)
    (inner-compile expr nil))
