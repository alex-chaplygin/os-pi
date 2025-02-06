;; *inst-table* - список инструкций для генерации байт-кода.
(defvar *inst-table*
  '(const
    jmp jnt
    alloc
    global-ref global-set
    local-ref local-set
    deep-ref deep-set
    push
    reg-call return
    fix-closure save-env set-env restore-env
    pack
    prim nprim
    halt))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt fix-closure reg-call))
;; *consts* - список констант.
(defvar *consts*)
;; *func-args-hash* - хэш-таблица, в которой ключ - адрес функции, а значение - её тип и число аргументов.
(defvar *func-args-hash*)

;; Превращает список инструкций с метками в байт-код.
(defun assemble (program)
  (assemble-second-pass (assemble-first-pass program)))

;; Первый проход ассемблера
;; (замена мнемоник инструкций соответствующими байтами
;; и сбор информации о метках и переходах).
(defun assemble-first-pass (program)
  (setq *consts* (list t nil)
        *func-args-hash* (make-hash))
  (let ((bytecode nil)
        (pc 0)
        (jmp-labels nil)
        (jmp-addrs (make-hash)))
    (labels ((asm-emit (&rest bytes)
               (setq bytecode (append bytecode bytes)
                     pc (+ pc (list-length bytes)))))
      (app #'(lambda (inst)
               (case (car inst)
                 ('label (assemble-label inst))
                 ('const (assemble-const inst))
                 ('prim (assemble-prim inst))
                 ('nprim (assemble-prim inst))
                 (otherwise (setq jmp-labels (assemble-inst inst jmp-labels)))))
           program))
    (setq bytecode (append bytecode (list (list-search *inst-table* 'HALT)))
          pc (++ pc))
    (list bytecode pc jmp-labels jmp-addrs)))

;; Второй проход ассемблера
;; (замена меток переходов на относительные адреса).
(defun assemble-second-pass (first-pass-res)
  (let ((bytecode (nth first-pass-res 0))
        (bytecode-len (nth first-pass-res 1))
        (jmp-labels (nth first-pass-res 2))
        (jmp-addrs (nth first-pass-res 3)))
    (let ((bytecode-arr (make-array bytecode-len)))
      (foldl #'(lambda (pc elem)
                 (seta bytecode-arr pc elem)
                 (++ pc))
             0 bytecode)
      (foldl #'(lambda (_ addr-label)
                 (seta bytecode-arr
                       (car addr-label)
                       (- (get-hash jmp-addrs (cdr addr-label)) (-- (car addr-label)))))
             nil jmp-labels)
      bytecode-arr)))

;; Ассемблирование метки.
(defun assemble-label (inst)
  (set-hash jmp-addrs (cadr inst) pc)
  (let ((fun nil)
        (type nil))
    (cond
      ((setq fun (search-symbol *fix-functions* (cadr inst)))
       (setq type 'fix))
      ((setq fun (search-symbol *nary-functions* (cadr inst)))
       (setq res 'nary)))
    (when type
      (set-hash *func-args-hash* pc (cons type (caddr fun))))))

;; Ассемблирование инструкции загрузки константы в ACC.
(defun assemble-const (inst)
  (let* ((c (cadr inst))
         (i (list-search *consts* c)))
    (when (null i)
      (setq i (++ (list-length *consts*))
            *consts* (append *consts* (list c)))
      (decf i))
    (asm-emit (list-search *inst-table* 'CONST) i)))

;; Ассемблирование инструкции вызова примитива.
(defun assemble-prim (inst)
  (let* ((prim-type (car inst))
         (lst (case prim-type
                ('prim *fix-primitives*)
                ('nprim *nary-primitives*)
                (otherwise (error "Unreachable"))))
         (prim (search-symbol lst (cadr inst)))
         (prim-i (list-search lst prim)))
    (when (null prim-i)
      (error (concat "Unknown " (symbol-name prim-type)
                     ": " (symbol-name prim))))
    (asm-emit (list-search *inst-table* prim-type) prim-i)))

;; Ассемблирование инструкции общего вида.
(defun assemble-inst (inst jmp-labels)
  (let ((opcode (list-search *inst-table* (car inst))))
    (when (null opcode)
      (error (concat "Unknown inst: " (symbol-name (car inst)))))
    (when (contains *inst-jmp-table* (car inst))
      (setq jmp-labels
            (append jmp-labels
                    (list (cons (++ pc) (cadr inst))))))
    (when (not (null opcode))
      (asm-emit opcode)
      (app #'(lambda (elem)
               (asm-emit elem))
           (cdr inst))))
  jmp-labels)
