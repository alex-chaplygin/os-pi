;; *cur-bytecode* - хранит текущий исполняемый байт-код.
(defvar *cur-bytecode* nil)
;; *prim1-table* - таблица примитивов, принимающих 1 аргумент.
(defvar *prim1-table*
  #(car cdr
    sin cos
    intern symbol-name string-size inttostr code-char print putchar
    make-array array-size
    symbolp integerp))
;; *prim2-table* - таблица примитивов, принимающих 2 аргумента.
(defvar *prim2-table*
  #(cons rplaca rplacd
    + - * / % & bitor << >> equal > <
    char ;; concat?
    aref))
;; *prim3-table* - таблица примитивов, принимающих 3 аргумента.
(defvar *prim3-table*
  #(subseq
    seta))
;; Таблица примитивов с фиксированным числом аргументов
(defvar *fix-prim-table* (make-array (list-length *fix-primitives*)))
(foldl #'(lambda (i prim)
           (seta *fix-prim-table* i prim)
           (++ i))
       0 *fix-primitives*)
;; Таблица примитивов с переменным числом аргументов
(defvar *nary-prim-table* (make-array (list-length *nary-primitives*)))
(foldl #'(lambda (i prim)
           (seta *nary-prim-table* i prim)
           (++ i))
       0 *nary-primitives*)
;; *cur-prim-func* - функция для выполнения примитива.
(defvar *cur-prim-func* nil)

;; Регистры:

;; *acc* - хранит результат последней операции.
(defvar *acc*)
;; *globals-mem* - хранит значения глобальных переменных.
(defvar *globals-mem*)
;; *pc* - хранит адрес текущей выполняемой инструкции.
(defvar *pc* 0)
;; *stack-max* - размер стека.
(defvar *stack-len* 2048)
;; *stack* - стек общего назначения.
(defvar *stack* (make-array *stack-len*))
;; *stack-head* - указатель на вершину стека.
(defvar *stack-head* 0)
;; *env* - ссылка на текущий кадр окружения.
(defvar *env*)
;; *env-num* - номер глубины кадра окружения.
(defvar *env-num*)
;; *const-mem* - хранит значения констант.
(defvar *const-mem*)

;; Инструкции:

;; CONST i - поместить i-ое константное выражение из CONST-MEM в регистр ACC.
(defun const (i)
  (setq *acc* (aref *const-mem* i)
        *pc* (+ *pc* 2)))

;; JMP - безусловный относительный переход на адрес addr.
(defun jmp (addr)
  (setq *pc* (+ *pc* addr)))

;; JNT - если ACC != t, то относительный переход на адрес addr.
(defun jnt (addr)
  (if *acc*
      (setq *pc* (+ *pc* 2))
      (jmp addr)))

;; ALLOC n - создать новый кадр активации размером n.
(defun alloc (n)
  (let ((prev-env (stack-pop))
	(new-frame (cons *env* (make-array n))))
    (labels ((add-arg (i)
		      (when (>= i 0)
			(seta (cdr new-frame) i (stack-pop))
			(add-arg (-- i)))))
	    (add-arg (-- n)))
    (stack-push prev-env)
    (setq *env* new-frame
          *env-num* (++ *env-num*)
          *pc* (+ *pc* 2))))

;; GLOBAL-REF - загружает в ACC значение глобальной переменной с индексом i.
(defun global-ref (i)
  (setq *acc* (aref *globals-mem* i)
        *pc* (+ *pc* 2)))

;; GLOBAL-SET - присваивает глобальной переменной с индексом i значение регистра ACC.
(defun global-set (i)
  (seta *globals-mem* i *acc*)
  (setq *pc* (+ *pc* 2)))

;; LOCAL-REF - загружает в ACC значение i локальной переменной.
(defun local-ref (i)
  (setq *acc* (aref (cdr *env*) i)
        *pc* (+ *pc* 2)))

;; LOCAL-SET i - присваивает локальной переменной i значение регистра ACC.
(defun local-set (i)
  (seta (cdr *env*) i *acc*)
  (setq *pc* (+ *pc* 2)))

;; DEEP-REF i j - загружает в ACC значение локальной переменной с индексом j в кадре i.
(defun deep-ref (i j)
  (let ((env *env*))
    (while (> i 0)
      (setq env (car env)
            i (-- i)))
    (setq *acc* (aref (cdr env) j)
          *pc* (+ *pc* 3))))

;; DEEP-SET i j - присваивает локальной переменной j в кадре i значение регистра ACC.
(defun deep-set (i j)
  (let ((env *env*))
    (while (> i 0)
      (setq env (car env)
            i (-- i)))
    (seta (cdr env) j *acc*
          *pc* (+ *pc* 3))))

;; PUSH - добавляет значение регистра ACC в стэк.
(defun push ()
  (stack-push *acc*)
  (incf *pc*))

(defun stack-push (val)
  (seta *stack* *stack-head* val)
  (incf *stack-head*)
  (when (>= *stack-head* *stack-len*)
    (error "Stack overflow")))

(defun stack-pop ()
  (setq *stack-head* (- *stack-head* 1))
  (when (< *stack-head* 0)
    (error "Stack underflow"))
  (aref *stack* *stack-head*))

(defun stack-drop (n)
  (setq *stack-head* (- *stack-head* n))
  (when (< *stack-head* 0)
    (error "Stack underflow")))

(defun stack-peek (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (aref *stack* idx)))

;; REG-CALL addr - добавляет адрес следующей инструкции в стэк и производит переход на относительный адрес addr.
(defun reg-call (addr)
  (stack-push (+ *pc* 2))
  (jmp addr))

;; RETURN - производит переход на адрес из верхушки стэка, при этом удаляет этот адрес из стэка.
(defun return ()
  (setq *pc* (stack-pop)))

;; FIX-CLOSURE - в регистр ACC добавляется объект замыкание с текущим кадром активации и смещением на код функции относительно текущего адреса addr.
(defun fix-closure (addr)
  (setq *acc* (cons (+ *pc* addr) *env-num*)
        *pc* (+ *pc* 2)))

;; SAVE-ENV - сохраняет текуший кадр активации в стек.
(defun save-env ()
  (stack-push (cons *env* *env-num*))
  (incf *pc*))

;; SET-ENV num - сохраняет текущий кадр окружения в стек и производит переход окружения на кадр с номером num.
(defun set-env (num)
  (let ((env-diff (- *env-num* num)))
    (when (< env-diff 0)
      (error "SET-ENV: invalid operand"))
    (when (!= env-diff 0)
      (while (!= env-diff 0)
        (when (null *env*)
          (error "SET-ENV: invalid operand"))
        (setq *env* (car *env*)
              *env-num* (-- *env-num*)
              env-diff (-- env-diff))))
    (setq *pc* (+ *pc* 2))))

;; RESTORE-ENV - восстанавливает окружение из стека.
(defun restore-env ()
  (let ((env-pair (stack-pop)))
    (let ((env (car env-pair))
          (env-num (cdr env-pair)))
      (setq *env* env
            *env-num* env-num)
      (incf *pc*))))

;; PACK n - упаковка n верхних элементов в стеке в список.
(defun pack (n)
  (let ((lst ()))
    (for i 0 n
         (setq lst (cons (stack-pop) lst)))
    (stack-push lst)
    (setq *pc* (+ *pc* 2))))

;; Вызывает примитив из таблицы примитивов *fix-prim-table* с индексом i.
(defun prim (i)
  (let* ((cur-prim (aref *fix-prim-table* i))
         (args (cdr cur-prim)))
    (setq *cur-prim-func* (list (car cur-prim)))
    (while (!= args 0)
      (setq *cur-prim-func* (append *cur-prim-func* (list (list 'QUOTE (stack-peek (decf args)))))))
    (stack-drop (cdr cur-prim))
    (setq *acc* (vm-exec-prim)
          *pc* (+ *pc* 2))))

;; Особый случай примтива - вызов funcall.
(defun nprim-funcall ()
  (let* ((args (stack-pop))
         (closure (stack-pop))
         (addr (car closure))
         (env-num (cdr closure)))
    (when (not (and (pairp closure)
                    (integerp (car closure))
                    (integerp (cdr closure))))
      (error "funcall: invalid closure"))
    (let* ((fun (get-hash *func-args-hash* addr))
           (type (car fun))
           (count (cdr fun)))
      (when (or (and (equal type 'fix) (!= (list-length args) count))
                (and (equal type 'nary) (< (list-length args) count)))
        (error "funcall: invalid number of args")))
    (app #'(lambda (arg) (stack-push arg)) args)
    (save-env)
    (set-env env-num)
    (alloc (list-length args))
    (setq *pc* (- *pc* 7))
    (let ((ret-addr (+ *pc* 2)))
      (reg-call (- addr *pc*))
      (while (!= *pc* ret-addr)
        (vm-exec-inst)))
    (restore-env)
    (incf *pc*)))

;; Вызывает примитив из таблицы примитивов *nary-prim-table* с индексом i.
(defun nprim (i)
  (if (equal (car (aref *nary-prim-table* i)) 'funcall)
      (nprim-funcall)
      (let ((cur-prim (aref *nary-prim-table* i))
            (rest (map #'(lambda (arg) (list 'QUOTE arg)) (stack-pop)))
            (args ())
            (n 0))
        (setq n (cdr cur-prim))
        (while (!= n 0)
          (setq args (append args (list (list 'QUOTE (stack-peek (decf n)))))))
        (setq args (append args rest)
              *cur-prim-func* (cons (car cur-prim) args))
        (stack-drop (cdr cur-prim))
        (setq *acc* (vm-exec-prim)
              *pc* (+ *pc* 2)))))

;; Выполняет функцию примитива *cur-prim-func*
(defmacro vm-exec-prim ()
  *cur-prim-func*)

(defvar *insts*
  '((const . 1)
    (jmp . 1) (jnt . 1)
    (alloc . 1)
    (global-ref . 1) (global-set . 1)
    (local-ref . 1) (local-set . 1)
    (deep-ref . 2) (deep-set . 2)
    (push . 0)
    (reg-call . 1) (return . 0)
    (fix-closure . 1) (save-env . 0) (set-env . 1) (restore-env . 0)
    (pack . 1) (prim . 1) (nprim . 1)
    (halt . 0)))

(defun print-stack ()
  (labels ((collect-stack (i)
             (if (= i *stack-head*)
                 nil
                 (append (list (aref *stack* i)) (collect-stack (++ i))))))
    (print (list 'stack '= (collect-stack 0)))))

;; Выполняет текущую инструкцию.
(defun vm-exec-inst ()
  (let ((op1 (if (< (++ *pc*) (array-size *cur-bytecode*))
                 (aref *cur-bytecode* (++ *pc*)) nil))
        (op2 (if (< (+ *pc* 2) (array-size *cur-bytecode*))
                 (aref *cur-bytecode* (+ *pc* 2)) nil)))
    (case (aref *cur-bytecode* *pc*)
      (0  (funcall #'const op1))
      (1  (funcall #'jmp op1))
      (2  (funcall #'jnt op1))
      (3  (funcall #'alloc op1))
      (4  (funcall #'global-ref op1))
      (5  (funcall #'global-set op1))
      (6  (funcall #'local-ref op1))
      (7  (funcall #'local-set op1))
      (8  (funcall #'deep-ref op1 op2))
      (9  (funcall #'deep-set op1 op2))
      (10 (funcall #'push))
      (11 (funcall #'reg-call op1))
      (12 (funcall #'return))
      (13 (funcall #'fix-closure op1))
      (14 (funcall #'save-env))
      (15 (funcall #'set-env op1))
      (16 (funcall #'restore-env))
      (17 (funcall #'pack op1))
      (18 (funcall #'prim op1))
      (19 (funcall #'nprim op1))
      (20 t)
      (otherwise (error "Unknown instruction opcode")))))

;; Выполняет байт-код на виртуальной машине.
(defun vm-run (bytecode)
  (setq *cur-bytecode* bytecode
        *acc* nil
        *pc* 0
        *stack-head* 0
        *env* nil
        *env-num* 0
        *globals-mem* (make-array *global-variables-count*)
        *const-mem* (make-array (list-length *consts*)))
  (for i 0 (array-size *const-mem*)
       (seta *const-mem* i (nth *consts* i)))
  (seta *globals-mem* 0 0)
  (seta *globals-mem* 1 1)
  (let ((halt (list-search *insts* (cons 'HALT 0))))
    (while (!= (aref *cur-bytecode* *pc*) halt)
      (let* ((cur-inst-pair (nth *insts* (aref *cur-bytecode* *pc*)))
             (cur-inst (car cur-inst-pair))
             (cur-inst-args-len (cdr cur-inst-pair)))
        (print (case cur-inst-args-len
                 (0 (list '--> *pc* cur-inst))
                 (1 (list '--> *pc* cur-inst (aref *cur-bytecode* (++ *pc*))))
                 (2 (list '--> *pc* cur-inst (aref *cur-bytecode* (++ *pc*)) (aref *cur-bytecode* (+ *pc* 2)))))))
      (vm-exec-inst)
      (print-stack)
      (print (list 'env '= *env*))
      (print (list 'acc '= *acc*))
      (print '-----)))
  *acc*)
