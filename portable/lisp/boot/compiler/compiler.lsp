;; *globals* - список имён глобальных переменных.
(defvar *globals* nil)
;; *globals-count* - число глобальных переменных в списке *globals*.
(defvar *globals-count* 0)
;; *global-functions* - глобальное окружение функций, содержащий названия функций и кол-во аргументов.
(defvar *global-functions* nil)
;; постоянный список примитивов с количеством их аргументов
(defvar *primitives* '((car . 1) (cdr . 1) (cons . 2)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg)
  (return-from compiler msg))

;; Расширить окружение новым кадром аргументов
(defun extend-env (env args)
  (cons args env))

;; Добавить глобальную функцию с именем и числом аргументов
(defun add-func (name arity)
  (setq *global-functions* (cons (cons name arity) *global-functions*)))

;; Поиск функции или примитива по имени, возвращет кол-во аргументов или nil - если не найдено
(defun search-func-list (list name)
  (labels ((search (list)
	     (if (null list) nil
		 (if (eq (caar list) name) (cdar list)
		     (search (cdr list))))))
    (search list)))

;; Проверка на правильность lambda выражения, или ошибка
(defun correct-lambda (f)
  t)

;; Комплирует лямбда-абстракцию.
;; (список аргументов, тело функции, локальное окружение).
(defun compile-lambda (name args body env)
  (let ((arity (list-length args)))
    (add-func name arity)
    (list 'LABEL name
	  (list 'FIX-CLOSURE arity
		(inner-compile body (extend-env env args))))))

;; Определения типа функции: лямбда, примитив, глобальная функция
(defun find-func (f)
  (if (and (not (atom f)) (correct-lambda f))
      (cons 'lambda (gensym))
      (let ((r (search-func-list *global-functions* f)))
	(if r (cons 'func r)
	    (let ((r (search-func-list *primitives* f)))
	      (if r (cons 'primitive r)
		  (comp-err (concat "unknown function " (symbol-name f)))))))))
		  
;; Применение функции
;; f - имя функции или lambda, args - аргументы, env - окружение
(defun compile-application (f args env)
  (let* ((fun (find-func f))
	 (type (car fun)))
    (print `(app ,f ,args ,fun))
    (if (eq type 'lambda)
	(compile-lambda (cdr fun) (cadr f) (caddr f) env)
	(let ((count (cdr fun)))
	  (if (!= count (list-length args))
	      (comp-err (concat "invalid args count"))
	      (let ((vals (map #'(lambda (a) (inner-compile a env)) args)))
		(if (eq type 'func)
		    (list 'SEQ (list 'ALLOC count) (list 'REG-CALL f vals))
		    (list 'PRIM f vals))))))))

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
  (setq *globals* (append *globals* (list sym)))
  (incf *globals-count*)
  (- *globals-count* 1))

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
  (let ((i (list-search *globals* var)))
    (if (null i) nil
	(list 'global i))))

;; Производит поиск переменной в локальном окружении по символу.
;; Возвращает индекс переменной в стеке, если символ найден, иначе nil.
(defun find-local-var (var env)
  (labels ((find-frame (frames i)
	     (if (null frames) nil
		 (let ((j (list-search (car frames) var)))
		   (if (null j) (find-frame (cdr frames) (++ i))
		       (list (if (= i 0) 'local 'deep) i j))))))
	  (find-frame env 0)))

;; Компиляция перемнной
(defun compile-variable (v env)
  (let ((res (find-var v env)))
    (if (null res)
	(comp-err `(Unknown symbol ,v))
      (case (car res)
	    ('local (list 'LOCAL-REF (cadr res)))
	    ('global (list 'GLOBAL-REF (cadr res)))
	    ('deep (list 'DEEP-REF (cadr res) (caddr res)))))))

;; Компиляция константы
(defun compile-constant (c)
  (list 'CONST c))

;; Функция компиляции в промежуточную форму
;; expr - выражение, env - лексическое окружение
(defun inner-compile (expr env)
    (if (atom expr)
        (if (symbolp expr)
	    (compile-variable expr env)
	  (compile-constant expr))
        (let ((func (car expr))
              (args (cdr expr)))
          (case func
            ('progn (compile-progn args env))
            ('if (compile-if args env))
            ('setq (compile-setq args env))
            ('defun (compile-defun args env))
            (otherwise (compile-application func args env))))))

;; Создаёт список инструкций для вычисления S-выражения expr на виртуальной машине с помощью функции vm-run.
;; expr - S-выражение
(defun compile (expr)
  (setq *globals* '(t nil)
        *globals-count* (list-length *globals*)
        *comp-err* nil
        *comp-err-msg* nil
        *global-functions* nil
        *environment* nil)
  (block compiler
    (inner-compile `(progn ,@expr) nil)))


;; Компилирует вызов функции.
;; label - метка тела функции.
;; fparams - параметры функции.

;; Компилирует вызов примитива.
;; args - аргументы примитива.
;; prim-i - индекс в таблице примитивов.
