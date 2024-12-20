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

;; *acc* - хранит результат последней операции.
(defvar *acc* nil)
;; *globals-mem* - хранит значения глобальных переменных.
(defvar *globals-mem* nil)
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

;; Инструкции:

;; CONST expr - поместить S-выражение expr в регистр ACC.
(defun const (expr)
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
  (const (aref *globals-mem* i)))

;; GLOBAL-SET - присваивает глобальной переменной с индексом i значение регистра ACC.
(defun global-set (i)
  (seta *globals-mem* i *acc*)
  (setq *pc* (+ *pc* 2)))

;; LOCAL-REF - загружает в ACC значение i локальной переменной.
(defun local-ref (i)
  (const (aref (cdr *env*) i)))

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

;; SET-ENV num - сохраняет текущий кадр окружения в стек и производит переход окружения на кадр с номером num.
(defun set-env (num)
  (let ((env-diff (- *env-num* num)))
    (when (< env-diff 0)
      (error "SET-ENV: invalid operand"))
    (stack-push (cons *env* *env-num*))
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

;; PRIM1 - вызывает примитив от 1 аргумента по индексу i в таблице примитивов *prim1-table*.
(defun prim1 (i)
  (setq *cur-prim-func*
        (list (aref *prim1-table* i)
              (stack-peek 0)))
  (const (vm-exec-prim))
  (stack-drop 1))

;; PRIM2 - вызывает примитив от 2 аргументов по индексу i в таблице примитивов *prim2-table*.
(defun prim2 (i)
  (setq *cur-prim-func*
        (list (aref *prim2-table* i)
	      ;; (stack-peek 1)
              ;; (stack-peek 0)
              ;; (list 'quote (stack-peek 1))
              ;; (list 'quote (stack-peek 0))
	      (if (null (stack-peek 1)) nil (list 'quote (stack-peek 1)))
	      (if (null (stack-peek 0)) nil (list 'quote (stack-peek 0)))
	      ))
  (const (vm-exec-prim))
  (stack-drop 2))

;; PRIM3 - вызывает примитив от 3 аргументов по индексу i в таблице примитивов *prim3-table*.
(defun prim3 (i)
  (setq *cur-prim-func*
        (list (aref *prim3-table* i)
              (stack-peek 2)
              (stack-peek 1)
              (stack-peek 0)))
  (const (vm-exec-prim))
  (stack-drop 3))

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
    (fix-closure . 0) (set-env . 1) (restore-env . 0)
    (prim1 . 1) (prim2 . 1) (prim3 . 1)))

(defun print-stack ()
  (labels ((collect-stack (i)
             (if (= i *stack-head*)
                 nil
                 (append (list (aref *stack* i)) (collect-stack (++ i))))))
    (print (list 'stack '= (collect-stack 0)))))

;; Выполняет байт-код на виртуальной машине.
(defun vm-run (bytecode)
  (setq *acc* nil
        *pc* 0
        *stack-head* 0
        *env* nil
        *env-num* 0)
  (setq *globals-mem*
        (make-array *global-variables-count*))
  (seta *globals-mem* 0 t)
  (seta *globals-mem* 1 nil)
  (let ((bytecode-len (array-size bytecode)))
    (while (< *pc* bytecode-len)
      (let ((op1 (if (< (++ *pc*) bytecode-len)
                     (aref bytecode (++ *pc*)) nil))
            (op2 (if (< (+ *pc* 2) bytecode-len)
                     (aref bytecode (+ *pc* 2)) nil)))
        (let* ((cur-inst-pair (nth *insts* (aref bytecode *pc*)))
               (cur-inst (car cur-inst-pair))
               (cur-inst-args-len (cdr cur-inst-pair)))
          (print (case cur-inst-args-len
                   (0 (list '--> *pc* cur-inst))
                   (1 (list '--> *pc* cur-inst (aref bytecode (++ *pc*))))
                   (2 (list '--> *pc* cur-inst (aref bytecode (++ *pc*)) (aref bytecode (+ *pc* 2)))))))
        (case (aref bytecode *pc*)
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
          (13 nil) ;; fix-closure
          (14 (funcall #'set-env op1))
          (15 (funcall #'restore-env))
          (16 (funcall #'prim1 op1))
          (17 (funcall #'prim2 op1))
          (18 (funcall #'prim3 op1))
          (otherwise (error "Unknown instruction opcode")))
        (print-stack)
        (print (list 'env '= *env*))
        (print (list 'acc '= *acc*))
        (print '-----)
        )))
  *acc*)
