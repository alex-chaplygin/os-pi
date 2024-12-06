;; *globals* - список имён глобальных переменных.
(defvar *globals* nil)
;; *globals-count* - число глобальных переменных в списке *globals*.
(defvar *globals-count* 0)
;; *comp-err* - флаг, произошла ли ошибка при последней компиляции.
(defvar *comp-err* nil)
;; *comp-err-msg* - сообщение о последней ошибки компиляции.
(defvar *comp-err-msg* nil)
;; *funcs* - список пользовательских функций, содержащий названия аргументов.
(defvar *funcs* nil)
;; *environment* - окружение переменных как список кадров. ((X) (Y) (F))
(defvar *environment* nil)


;; Создаёт список инструкций для вычисления S-выражения expr на виртуальной машине с помощью функции vm-run.
;; expr - S-выражение
(defun compile (expr)
  (setq *globals* nil
        *globals-count* 0
        *comp-err* nil
        *comp-err-msg* nil
        *funcs* nil
        *environment* nil)
  (inner-compile expr))

(defun inner-compile (expr)
    (if (atom expr)
        (if (symbolp expr)
	    (compile-variable expr)
	  (compile-constant expr))
        (let ((func (car expr))
              (args (cdr expr)))
          (case func
            ('progn (compile-progn args))
            ('if (compile-if args))
            ('setq (compile-setq args))
            ('defun (compile-defun args))
            (otherwise
	     (compile-application func args))))))


(defun compile-constant (c)
  (list 'CONST c))

(defun compile-variable (v)
  (let ((res (find-var v)))
    (if (null res)
	(comp-err `(Unknown symbol ,v))
      (case (car res)
	    ('local (list 'LOCAL-FETCH (cadr res)))
	    ('global (list 'GLOBAL-FETCH (cadr res)))
	    ('deep (list 'DEEP-FETCH (cadr res) (caddr res)))))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst)
  (if (null lst) (compile-const nil)
    (if (null (cdr lst))
	(inner-compile (car lst))
      (list 'SEQ (inner-compile (car lst)) (compile-progn (cdr lst))))))

;; Компилирует if-выражение.
;; if-body - тело if-выражения (условие, ветка по "Да" и по "Нет").
(defun compile-if (expr)
  (if (!= (list-length expr) 3)
      (comp-err "if: invalid params")
    (let ((cond (inner-compile (car expr)))
	  (true (inner-compile (cadr expr)))
	  (false (inner-compile (caddr expr))))
      (list 'ALTER cond true false))))

;; Компилирует setq-выражение.
;; setq-body - пара из символа и выражения, которое необходимо установить этому символу.
(defun compile-setq (body)
  (if (null body)
      (comp-err "setq: no params")
    (labels ((compile-all-setq (body)
			       (list 'SEQ (compile-single-setq body) (compile-all-setq (cddr body))))
	     (compile-single-setq (body)
	      (if (< (list-length body) 2)
		  (comp-err "setq: no expression to set")
		(let* ((setq-sym (car setq-body))
		       (val (inner-compile (cadr setq-body)))
		       (find-res (find-var setq-sym))
		       (if (not (symbolp setq-sym))
			   (comp-err (concat "setq: invalid variable: " (symbol-name setq-sym)))
			   (if (null find-res)
			     (add-global setq-sym))
			   (case (car find-res)
				 ('global (list 'GLOBAL-SET (cadr find-res) val))
				 ('local (list 'LOCAL-SET (cadr find-res) val))
				 ('deep (list 'DEEP-SET (cadr find-res) (caddr find-res) val))
				 (otherwise (error "Unreachable")))))))))
	    (compile-all-setq body))))

(defun add-global (sym)
  (setq *globals* (append *globals* (list sym)))
  (incf *globals-count*)
  (- *globals-count 1))

(defun compile-application (f args)
  (let* ((func (compile-func f))
	 (arity (func-arity func)))
    (if (!= arity (list-length args))
	(comp-err "apply: invalid arity")
      (let ((call-node (list 'CALL func)))
	(if (= arity 0)
	    call-node
	  (list 'SEQ
		(list 'ALLOC arity)
		(let ((vals (map #'inner-compile args)))
		  (labels ((add-vals (vals i)
				     (let ((cur-node (list 'LOCAL-SET i (car vals))))
				       (if (null (cdr vals)) cur-node
					 (list 'SEQ cur-node (add-vals (cdr vals) (++ i))))))
			   (add-vals vals 0))))
		call-node))))))


;; Компилирует объявление функции (lambda или defun).
;; func-name - название функции (nil в случае lambda).
;; func-args - список названий переменных.
;; func-body - тело функции.
;; Возвращает метку на тело скомпилированной функции.
(defun compile-func (func-name func-args func-body)
  (let ((locals-copy *locals*)
        (label-func (gensym))
        (label-after (gensym)))
    (setq *funcs*
          (cons `(,func-name ,label-func ,args) *funcs*))
    (emit `(jmp ,label-after))
    (emit label-func)
    (foldr #'(lambda (_ arg)
               (setq *locals* (cons arg *locals*)))
           nil args)
    (setq *locals* (cons nil *locals*))
    (dolist (expr func-body)
      (inner-compile expr))
    (setq *locals* locals-copy)
    (emit '(ret))
    (emit label-after)
    label-func))

;; Комплирует лямбда-выражение.
;; lambda-body - тело лямбда-выражения (список аргументов и тело функции).
;; Возвращает метку на тело скомпилированной лямбда-функции.
(defun compile-lambda (lambda-body)
  (if (null lambda-body)
      (comp-err "No params in lambda")
      (if (null (cdr lambda-body))
          (comp-err "No body in lambda")
          (let ((args (car lambda-body))
                (lbody (cdr lambda-body)))
            (if (and (not (null args))
                     (atom args))
                (comp-err "Invalid params in lambda")
                (progn
                  (dolist (arg args)
                    (when (not (symbolp arg))
                      (comp-err "Not symbol in lambda args")))
                  (unless *comp-err*
                    (compile-func nil args lbody))))))))

;; Компилирует объявление функции с помощью DEFUN.
;; defun-body - список, состоящий из названия, списка аргументов и тела функции.
(defun compile-defun (defun-body)
  (if (null defun-body)
      (comp-err "No name in defun")
      (if (null (cdr defun-body))
          (comp-err "No args in defun")
          (let ((name (car defun-body))
                (args (cadr defun-body))
                (fbody (cddr defun-body)))
            (if (null fbody)
                (comp-err "No body in defun")
                (if (symbolp name)
                    (if (and (not (null args))
                             (atom args))
                        (comp-err "Invalid params in defun")
                        (progn
                          (dolist (arg args)
                            (when (not (symbolp arg))
                              (comp-err "Not symbol in defun args")))
                          (unless *comp-err*
                            (compile-func name args fbody))))
                    (comp-err "Not symbol in defun function name")))))))

;; Компилирует вызов функции.
;; label - метка тела функции.
;; fparams - параметры функции.
(defun compile-func-call (label fparams)
  (let ((func (foldl #'(lambda (res f)
                         (if (and (null res)
                                  (eq (cadr f) label))
                             f
                             res))
                     nil *funcs*))
        (args nil)
        (args-len 0)
        (fparams-len (foldl #'(lambda (len _)
                                (++ len))
                            0 fparams)))
    (when (null func)
      (error "Unreachable"))
    (setq args (caddr func))
    (dolist (_ args)
      (setq args-len (++ args-len)))
    (if (not (eq fparams-len args-len))
        (comp-err (concat
                   "Invalid number of arguments (expected "
                   (inttostr args-len)
                   ", but got " (inttostr fparams-len) ")"))
        (progn
          (foldr #'(lambda (_ param)
                     (inner-compile param)
                     (emit (list 'push)))
                 nil fparams)
          (emit `(call ,label))
          (when (> args-len 0)
            (emit `(drop ,args-len)))))))

;; Компилирует вызов примитива.
;; args - аргументы примитива.
;; prim-i - индекс в таблице примитивов.
(defun compile-prim (args prim-i)
  (let* ((args-len (list-length args))
         (inst (case args-len
                 (1 'prim1)
                 (2 'prim2)
                 (3 'prim3)
                 (otherwise (error "Unreachable"))))
         (locals-copy *locals*))
    (foldr #'(lambda (_ expr)
               (inner-compile expr)
               (emit (list 'push))
               (setq *locals* (cons nil *locals*)))
           nil args)
    (emit (list inst prim-i))
    (setq *locals* locals-copy)))

;; Добавляет инструкцию к текущей накопленной программе *program*.
(defun emit (val)
  (setq *program* (append *program* (list val))))

;; Производит поиск переменной по символу сначала в локальном, а затем в глобальном окружении.
;; Возвращает список, состоящий из символа - GLOBAL или LOCAL, DEEP,
;; и индекса в массиве глобального окружения или в стеке, либо nil.
(defun find-var (var)
  (let ((local (find-local-var var)))
    (if (not (null local)) local)
        (find-global-var var)))

;; Производит поиск переменной в глобальном окружении по символу.
;; Возвращает индекс в массиве глобального окружения, если символ найден, иначе nil.
(defun find-global-var (var)
  (labels ((find (list i)
		 (if (null list) nil
		   (if (eq (car list) var)
		       (cons 'global i)
		     (find (cdr list) (++ i))))))
    (find *globals* 0)))

;; Производит поиск переменной в локальном окружении по символу.
;; Возвращает индекс переменной в стеке, если символ найден, иначе nil.
(defun find-local-var (var)
  (labels ((find (list i j)
		 (if (null list) nil
		   (if (eq (car list) var)
		       (list (if (= i 0) 'local 'deep) i j)
		     (find (cdr list) i (++ j)))))
	   (find-frame (frames i)
		       (if (null frames) nil
			 (let ((res (find (car frames) 0 i)))
			   (if (null res)
			       (find-frame (cdr frames) (++ i) nil)
			     res)))))
	  (find-frame *environment* 0)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg)
  (setq *comp-err* t
	*comp-err-msg* msg))
