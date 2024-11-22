;; *cur-bytecode* - хранит текущий исполняемый байт-код.
(defvar *cur-bytecode* nil)
;; *cur-inst* - хранит текущую инструкцию для выполнения макросом inner-vm-exec-inst.
(defvar *cur-inst* nil)
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
;; *cur-prim-func* - функция для выполнения примитива.
(defvar *cur-prim-func* nil)

;; Регистры:

;; ACC - хранит результат последней операции.
(defvar *acc* nil)
;; GLOBALS-MEM - хранит значения глобальных переменных.
(defvar *globals-mem* nil)
;; PC - хранит адрес текущей выполняемой инструкции.
(defvar *pc* 0)
;; *stack-max* - размер стека.
(defvar *stack-max* 2048)
;; STACK - стек общего назначения.
(defvar *stack* (make-array *stack-max*))
;; *stack-head* - указатель на вершину стека.
(defvar *stack-head* 0)

;; Инструкции:

;; LDA - поместить S-выражение expr в регистр ACC.
(defun lda (expr)
  (setq *acc* expr
        *pc* (+ *pc* 2)))

;; JMP - безусловный относительный переход на адрес addr.
(defun jmp (addr)
  (setq *pc* (+ *pc* addr)))

;; JNT - если ACC != t, то относительный переход на адрес addr.
(defun jnt (addr)
  (if *acc*
      (setq *pc* (+ *pc* 2))
      (jmp addr)))

;; GLOBAL-SET - присвоить глобальной переменной с индексом i значение регистра ACC.
(defun global-set (i)
  (seta *globals-mem* i *acc*)
  (setq *pc* (+ *pc* 2)))

;; GLOBAL-GET - присвоить регистру ACC значение глобальной переменной с индексом i.
(defun global-get (i)
  (lda (aref *globals-mem* i)))

;; PUSH - добавляет значение регистра ACC в стэк.
(defun push ()
  (stack-push *acc*)
  (setq *pc* (+ *pc* 1)))

(defun stack-push (val)
  (seta *stack* *stack-head* val)
  (setq *stack-head* (++ *stack-head*))
  (when (>= *stack-head* *stack-max*)
    (error "Stack overflow")))

;; POP - загружает верхний элемент стека в регистр ACC, при этом удаляет этот элемент из стека.
(defun pop ()
  (lda (stack-pop)))

(defun stack-pop ()
  (setq *stack-head* (- *stack-head* 1))
  (when (< *stack-head* 0)
    (error "Stack underflow"))
  (aref *stack* *stack-head*))

;; DROP n - удаляет n верхних элементов из стека.
(defun drop (n)
  (stack-drop n)
  (setq *pc* (+ *pc* 2)))

(defun stack-drop (n)
  (setq *stack-head* (- *stack-head* n))
  (when (< *stack-head* 0)
    (error "Stack underflow")))

;; LOCAL-GET - загружает в ACC значение i-го элемента стэка, начиная с верхушки.
(defun local-get (i)
  (lda (stack-peek i)))

(defun stack-peek (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (aref *stack* idx)))

;; LOCAL-SET i - присваивает элементу стека с индексом i, начиная с верхушки, значение регистра ACC.
(defun local-set (i)
  (let ((idx (- (- *stack-head* i) 1)))
    (when (< idx 0)
      (error "Stack underflow"))
    (seta *stack* idx *acc*)
    (setq *pc* (+ *pc* 2))))

;; CALL addr - добавляет адрес следующей инструкции в стэк и производит переход на относительный адрес addr.
(defun call (addr)
  (stack-push (+ *pc* 2))
  (jmp addr))

;; RET - производит переход на адрес из верхушки стэка, при этом удаляет этот адрес из стэка.
(defun ret ()
  (jmp (- (stack-pop) *pc*)))

;; PRIM1 - вызывает примитив от 1 аргумента по индексу i в таблице примитивов *prim1-table*.
(defun prim1 (i)
  (when (>= i (array-size *prim1-table*))
    (error (concat "Unknown prim1: " (inttostr i))))
  (setq *cur-prim-func*
        `(,(aref *prim1-table* i)
           ,(stack-peek 0)))
  (lda (vm-exec-prim))
  (stack-drop 1))

;; PRIM2 - вызывает примитив от 2 аргументов по индексу i в таблице примитивов *prim2-table*.
(defun prim2 (i)
  (when (>= i (array-size *prim2-table*))
    (error (concat "Unknown prim2: " (inttostr i))))
  (setq *cur-prim-func*
        `(,(aref *prim2-table* i)
           ,(stack-peek 0)
           ,(stack-peek 1)))
  (lda (vm-exec-prim))
  (stack-drop 2))

;; PRIM3 - вызывает примитив от 3 аргументов по индексу i в таблице примитивов *prim3-table*.
(defun prim3 (i)
  (when (>= i (array-size *prim3-table*))
    (error (concat "Unknown prim3: " (inttostr i))))
  (setq *cur-prim-func*
        `(,(aref *prim3-table* i)
           ,(stack-peek 0)
           ,(stack-peek 1)
           ,(stack-peek 2)))
  (lda (vm-exec-prim))
  (stack-drop 3))

;; Выполняет функцию примитива *cur-prim-func*
(defmacro vm-exec-prim ()
  *cur-prim-func*)


;; Выполняет программу program.
;; bytecode - массив инструкций (пар опкодов и операндов).
;; Возвращает значение регистра ACC.
(defun vm-run (bytecode)
  (setq *cur-bytecode* bytecode
        *acc* nil
        *pc* 0
        *stack-head* 0)
  (when (> *globals-count* 0)
    (setq *globals-mem*
          (make-array *globals-count*)))
  (let ((bytecode-len (array-size bytecode)))
    (while (< *pc* bytecode-len)
      (case (aref bytecode *pc*)
        (0 (vm-exec-inst 'lda 1))
        (1 (vm-exec-inst 'jmp 1))
        (2 (vm-exec-inst 'jnt 1))
        (3 (vm-exec-inst 'global-set 1))
        (4 (vm-exec-inst 'global-get 1))
        (5 (vm-exec-inst 'push 0))
        (6 (vm-exec-inst 'pop 0))
        (7 (vm-exec-inst 'drop 1))
        (8 (vm-exec-inst 'local-get 1))
        (9 (vm-exec-inst 'local-set 1))
        (10 (vm-exec-inst 'call 1))
        (11 (vm-exec-inst 'ret 0))
        (12 (vm-exec-inst 'prim1 1))
        (13 (vm-exec-inst 'prim2 1))
        (14 (vm-exec-inst 'prim3 1))
        (otherwise (error (concat
                           "Unknown opcode: "
                           (inttostr (aref bytecode *pc*)))))))
    *acc*))

;; Выполняет инструкцию inst с args-len операндами.
(defun vm-exec-inst (inst args-len)
  (let ((inst-args nil))
    (for i 1 (+ args-len 1)
         (setq inst-args
               (append inst-args
                       `(,(aref *cur-bytecode*
                                (+ *pc* i))))))
    (setq *cur-inst*
          (append `(,inst) inst-args))
    (inner-vm-exec-inst)))

(defmacro inner-vm-exec-inst ()
  *cur-inst*)
