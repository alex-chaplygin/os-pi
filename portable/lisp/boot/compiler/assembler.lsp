;; *inst-table* - список инструкций для генерации байт-кода.
(defvar *inst-table*
  '(const
    jmp jnt
    alloc
    global-ref global-set
    local-ref local-set
    deep-ref deep-set
    push
    pack
    reg-call return
    fix-closure save-env set-env restore-env
    prim nprim
    halt
    prim-closure
    nprim-closure
    catch
    throw))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt fix-closure reg-call catch))
;; *consts* - список констант.
(defvar *consts*)
;; *consts-end* - указатель на конец *consts*
(defvar *consts-end*)
;; *func-args-hash* - хэш-таблица, в которой ключ - адрес функции, а значение - её тип и число аргументов.
(defvar *func-args-hash*)
;; адреса где нужно изменить переходы с абсолютных на относительные
(defvar jmp-addrs)
;; счетчик комманд
(defvar pc)
;; генерируемый код
(defvar bytecode)
;; конец списка генерируемого кода
(defvar bytecode-end)

(defun last-cdr (list)
";; Найти последний cdr в списке"
  (if (null (cdr list)) list
      (last-cdr (cdr list))))

(defun asm-emit (&rest bytes)
";; Запись кода в список"
  (if (null bytecode)
      (setq bytecode bytes)
      (rplacd bytecode-end bytes))
  (setq bytecode-end (last-cdr bytes)
	pc (+ pc (list-length bytes))))

(defun assemble-label (inst)
";; Ассемблирование метки."
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

(defun assemble-const (inst)
";; Ассемблирование инструкции загрузки константы в ACC."
  (let* ((c (cadr inst))
	 (lc (list c))
         (i (list-search *consts* c)))
    (when (null i)
      (setq i (++ (list-length *consts*)))
      (rplacd *consts-end* lc)
      (setq *consts-end* lc)
      (decf i))
    (asm-emit (list-search *inst-table* 'CONST) i)))

(defun assemble-prim (inst)
";; Ассемблирование инструкции вызова примитива."
  (let* ((prim-type (car inst))
         (lst (case prim-type
                ('prim *fix-primitives*)
                ('prim-closure *fix-primitives*)
                ('nprim *nary-primitives*)
                ('nprim-closure *nary-primitives*)
                (otherwise (error "assemble-prim: invalid prim"))))
         (prim (search-symbol lst (cadr inst)))
         (prim-i (list-search lst prim)))
    (when (null prim-i)
      (error (concat "Unknown " (symbol-name prim-type)
                     ": " (symbol-name prim))))
    (asm-emit (list-search *inst-table* prim-type) prim-i)))

(defun assemble-inst (inst jmp-labels)
";; Ассемблирование инструкции общего вида."
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

(defun assemble-first-pass (program)
";; Первый проход ассемблера"
;; (замена мнемоник инструкций соответствующими байтами
;; и сбор информации о метках и переходах).
  (setq *consts* (list t nil)
	*consts-end* (cdr *consts*)
        *func-args-hash* (make-hash)
	jmp-addrs (make-hash)
	pc 0
	bytecode nil
	bytecode-end nil)
  (let ((jmp-labels nil))
    (app #'(lambda (inst)
	     (cond
	       ((contains '(prim nprim prim-closure nprim-closure) (car inst))
		(assemble-prim inst))
	       (t (case (car inst)
		    ('label (assemble-label inst))
		    ('const (assemble-const inst))
		    (otherwise (setq jmp-labels (assemble-inst inst jmp-labels)))))))
	 program)
    (asm-emit (list-search *inst-table* 'HALT))
    (setq pc (++ pc))
    (list bytecode pc jmp-labels)))

(defun assemble-second-pass (first-pass-res)
";; Второй проход ассемблера"
;; (замена меток переходов на относительные адреса).
  (let ((bytecode (car first-pass-res))
        (bytecode-len (second first-pass-res))
        (jmp-labels (third first-pass-res)))
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

(defun assemble (program)
";; Превращает список инструкций с метками в байт-код."
  (assemble-second-pass (assemble-first-pass program)))
