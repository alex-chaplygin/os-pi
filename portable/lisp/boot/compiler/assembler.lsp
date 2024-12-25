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
    prim1 prim2 prim3))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt reg-call))

;; Превращает список инструкций с метками в байт-код.
(defun assemble (program)
  (assemble-second-pass (assemble-first-pass program)))

;; Первый проход ассемблера
;; (замена мнемоник инструкций соответствующими байтами
;; и сбор информации о метках и переходах).
(defun assemble-first-pass (program)
  (let ((bytecode nil)
        (pc 0)
        (jmp-labels nil)
        (jmp-addrs (make-hash)))
    (labels ((asm-emit (&rest bytes)
               (setq bytecode (append bytecode bytes)
                     pc (+ pc (list-length bytes)))))
      (app #'(lambda (inst)
               (case (car inst)
                 ('label (set-hash jmp-addrs (cadr inst) (++ pc)))
                 ('prim (assemble-prim inst))
                 (otherwise (setq jmp-labels (assemble-inst inst jmp-labels)))))
           program))
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
                       (- (get-hash jmp-addrs (cdr addr-label)) (car addr-label))))
             nil jmp-labels)
      bytecode-arr)))

;; Ассемблирование инструкции вызова примитива.
(defun assemble-prim (inst)
  (let* ((prim (search-symbol *primitives* (cadr inst)))
         (prim-table (case (cdr prim)
                       (1 *prim1-table*)
                       (2 *prim2-table*)
                       (3 *prim3-table*)
                       (otherwise (error "Unreachable"))))
         (prim-i nil))
    (for i 0 (array-size prim-table)
         (when (eq (aref prim-table i) (car prim))
           (setq prim-i i
                 i (array-size prim-table))))
    (when (null prim-i)
      (error "Unreachable"))
    (asm-emit
     (list-search *inst-table*
                  (case (cdr  prim)
                    (1 'prim1)
                    (2 'prim2)
                    (3 'prim3)
                    (otherwise (error "Unreachable"))))
     prim-i)))

;; Ассемблирование инструкции общего вида.
(defun assemble-inst (inst jmp-labels)
  (let ((opcode (list-search *inst-table* (car inst))))
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
