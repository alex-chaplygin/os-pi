;; *program* - хранит накопленный результат компиляции функцией compile.
(defvar *program* nil)
;; *globals* - список имён глобальных переменных.
(defvar *globals* nil)
;; *globals-count* - число глобальных переменных в списке *globals*.
(defvar *globals-count* 0)
;; *comp-err* - флаг, произошла ли ошибка при последней компиляции.
(defvar *comp-err* nil)
;; *comp-err-msg* - сообщение о последней ошибки компиляции.
(defvar *comp-err-msg* nil)
;; *funcs* - список функций, содержащий названия аргументов.
(defvar *funcs* nil)
;; *locals* - окружение локальных переменных.
(defvar *locals* nil)


;; Создаёт список инструкций для вычисления S-выражения expr на виртуальной машине с помощью функции vm-run.
;; expr - S-выражение
(defun compile (expr)
  (setq *program* nil
        *globals* nil
        *globals-count* 0
        *comp-err* nil
        *comp-err-msg* nil
        *funcs* nil
        *locals* nil)
  (inner-compile expr)
  (if *comp-err*
      (progn
        (setq *comp-err-msg* (concat "[Compilation error] " *comp-err-msg*))
        ;; (print *comp-err-msg*)
        nil)
    *program*))

(defun inner-compile (expr)
  (unless *comp-err*
    (if (atom expr)
        (if (and (symbolp expr)
                 (not (eq expr t)))
            (let* ((find-res (find-var expr))
                   (scope (car find-res))
                   (var (cadr find-res)))
              (if (null var)
                  (comp-err (concat "Unknown symbol: " (symbol-name expr)))
                  (case scope
                    ('global (emit `(global-get ,var)))
                    ('local (emit `(local-get ,var)))
                    (otherwise (error "Unreachable")))))
            (emit `(lda ,expr)))
        (let ((func (car expr))
              (args (cdr expr)))
          (case func
            ('progn (compile-progn args))
            ('if (compile-if args))
            ('setq (compile-setq args))
            ('defun (compile-defun args))
            (otherwise
             (if (not (atom func))
                 (if (eq (car func) 'lambda)
                     (let ((label-func (compile-lambda (cdr func))))
                       (unless *comp-err*
                         (compile-func-call label-func args)))
                     (comp-err "Invalid lambda symbol"))
                 (let* ((args-len (list-length args))
                        (prim-table (case args-len
                                      (1 *prim1-table*)
                                      (2 *prim2-table*)
                                      (3 *prim3-table*)
                                      (otherwise nil)))
                        (prim-i nil))
                   (when (not (null prim-table))
                     (for i 0 (array-size prim-table)
                          (when (eq (aref prim-table i) func)
                            (setq prim-i i
                                  i (array-size prim-table)))))
                   (if (not (null prim-i))
                       (compile-prim args prim-i)
                       (let ((label-func (foldl '(lambda (res f)
                                                  (if (and (null res)
                                                           (eq (car f) func))
                                                      (cadr f)
                                                      res))
                                                nil *funcs*)))
                         (if (null label-func)
                             (comp-err (concat "Unknown func: " (symbol-name func)))
                             (compile-func-call label-func args))))))))))))

;; Компилирует блок progn.
;; lst - список S-выражений внутри блока progn.
(defun compile-progn (lst)
  (unless (null lst)
    (inner-compile (car lst))
    (compile-progn (cdr lst))))

;; Компилирует if-выражение.
;; if-body - тело if-выражения (условие, ветка по "Да" и по "Нет").
(defun compile-if (if-body)
  (if (null if-body)
      (comp-err "if: no params")
      (if (or
           (atom if-body)
           (null (cdr if-body))
           (null (cddr if-body)))
          (comp-err "if: not enough params")
          (if (not (null (cdr (cddr if-body))))
              (comp-err "if: too many params")
              (let ((if-cond (car if-body))
                    (if-true (cadr if-body))
                    (if-false (caddr if-body))
                    (label-false (gensym))
                    (label-after (gensym)))
                (inner-compile if-cond)
                (emit `(jnt ,label-false))
                (inner-compile if-true)
                (emit `(jmp ,label-after))
                (emit label-false)
                (inner-compile if-false)
                (emit label-after))))))

;; Компилирует setq-выражение.
;; setq-body - пара из символа и выражения, которое необходимо установить этому символу.
(defun compile-setq (setq-body)
  (if (null setq-body)
      (comp-err "setq: no params")
      (if (or
           (atom setq-body)
           (null (cdr setq-body)))
          (comp-err "setq: no expression to set")
          (let* ((setq-sym (car setq-body))
                 (setq-expr (cadr setq-body))
                 (find-res (find-var setq-sym))
                 (setq-var-type (car find-res))
                 (setq-i (cadr find-res)))
            (if (symbolp setq-sym)
                (if (eq setq-sym t)
                    (comp-err (concat "setq: variable name is constant: " (symbol-name setq-sym)))
                    (progn
                      (inner-compile setq-expr)
                      (when (null setq-i)
                        (when (not (eq setq-var-type 'global))
                          (error "Unreachable"))
                        (setq *globals* (append *globals* `(,setq-sym)))
                        (setq setq-i *globals-count*)
                        (setq *globals-count* (++ *globals-count*)))
                      (case setq-var-type
                        ('global (emit `(global-set ,setq-i)))
                        ('local (emit `(local-set ,setq-i)))
                        (otherwise (error "Unreachable")))
                      (when (not (null (cddr setq-body)))
                        (compile-setq (cddr setq-body)))))
                (comp-err "setq: variable name is not a symbol"))))))

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
    (foldr '(lambda (_ arg)
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
  (let ((func (foldl '(lambda (res f)
                       (if (and (null res)
                                (eq (cadr f) label))
                           f
                           res))
                     nil *funcs*))
        (args nil)
        (args-len 0)
        (fparams-len (foldl '(lambda (len _)
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
          (foldr '(lambda (_ param)
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
    (foldr '(lambda (_ expr)
           (inner-compile expr)
           (emit (list 'push))
           (setq *locals* (cons nil *locals*)))
         nil args)
    (emit `(,inst ,prim-i))
    (setq *locals* locals-copy)))

;; Добавляет инструкцию к текущей накопленной программе *program*.
(defun emit (val)
  (setq *program* (append *program* `(,val))))

;; Производит поиск переменной по символу сначала в локальном, а затем в глобальном окружении.
;; Возвращает список, состоящий из символа - GLOBAL или LOCAL,
;; и индекса в массиве глобального окружения или в стеке, либо nil.
(defun find-var (var)
  (let ((local (find-local-var var)))
    (if (not (null local)) `(local ,local)
        `(global ,(find-global-var var)))))

;; Производит поиск переменной в глобальном окружении по символу.
;; Возвращает индекс в массиве глобального окружения, если символ найден, иначе nil.
(defun find-global-var (var)
  (let ((globals *globals*)
        (i 0))
    (while (not (or (null globals)
                    (eq (car globals) var)))
      (setq globals (cdr globals))
      (setq i (++ i)))
    (if (null globals) nil i)))

;; Производит поиск переменной в локальном окружении по символу.
;; Возвращает индекс переменной в стеке, если символ найден, иначе nil.
(defun find-local-var (var)
  (let ((locals *locals*)
        (i 0))
    (while (not (or (null locals)
                    (eq (car locals) var)))
      (setq locals (cdr locals))
      (setq i (++ i)))
    (if (null locals) nil i)))

;; Устанавливает флаг ошибки компиляции и сохраняет сообщение об ошибке.
(defun comp-err (msg)
  (setq *comp-err* t)
  (setq *comp-err-msg* msg))
