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
    nprim-closure))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt fix-closure reg-call))
;; *consts* - список констант.
(defvar *consts*)
;; *consts-end* - указатель на конец *consts*
(defvar *consts-end*)
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
	*consts-end* (cdr *consts*)
        *func-args-hash* (make-hash))
  (let ((bytecode nil)
	(end nil)
        (pc 0)
        (jmp-labels nil)
        (jmp-addrs (make-hash)))
    (labels ((last-cdr (list)
	       (if (null (cdr list)) list
		   (last-cdr (cdr list))))
	     (asm-emit (&rest bytes)
	       (if (null bytecode)
		   (setq bytecode bytes)
		   (rplacd end bytes))
               (setq end (last-cdr bytes)
		     pc (+ pc (list-length bytes)))))
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
      (setq pc (++ pc)))
    (list bytecode pc jmp-labels jmp-addrs)))

;; Второй проход ассемблера
;; (замена меток переходов на относительные адреса).
(defun assemble-second-pass (first-pass-res)
  (let ((bytecode (car first-pass-res))
        (bytecode-len (second first-pass-res))
        (jmp-labels (third first-pass-res))
        (jmp-addrs (forth first-pass-res)))
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
	 (lc (list c))
         (i (list-search *consts* c)))
    (when (null i)
      (setq i (++ (list-length *consts*)))
      (rplacd *consts-end* lc)
      (setq *consts-end* lc)
      (decf i))
    (asm-emit (list-search *inst-table* 'CONST) i)))

;; Ассемблирование инструкции вызова примитива.
(defun assemble-prim (inst)
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
