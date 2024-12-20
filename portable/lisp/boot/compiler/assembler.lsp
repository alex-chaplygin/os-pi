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
    fix-closure set-env restore-env
    prim1 prim2 prim3))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt reg-call))

;; Превращает список инструкций с метками в байт-код.
(defun assemble (program)
  (let ((bytecode nil)
        (pc 0)
        (jmp-labels nil)
        (jmp-addrs (make-hash)))
    (labels ((asm-emit (&rest bytes)
               (setq bytecode (append bytecode bytes))))
      (app #'(lambda (inst)
               (case (car inst)
                 ('label (set-hash jmp-addrs (cadr inst) (++ pc)))
                 ('prim
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
                     prim-i)
                    (setq pc (+ pc 2))))
                 (otherwise
                  (let ((opcode (list-search *inst-table* (car inst))))
                    (when (contains *inst-jmp-table* (car inst))
                      (setq jmp-labels
                            (append jmp-labels
                                    (list (cons (++ pc) (cadr inst))))))
                    (when (not (null opcode))
                      (asm-emit opcode)
                      (app #'(lambda (elem)
                               (incf pc)
                               (asm-emit elem))
                           (cdr inst))
                      (incf pc))))))
           program))
    (let ((bytecode-arr (make-array pc)))
      (foldl #'(lambda (pc elem)
                 (seta bytecode-arr pc elem)
                 (++ pc))
             0 bytecode)
      (foldl #'(lambda (pc addr-label)
                 (seta bytecode-arr
                       (car addr-label)
                       (- (get-hash jmp-addrs (cdr addr-label)) (car addr-label))))
             0 jmp-labels)
      bytecode-arr)))
