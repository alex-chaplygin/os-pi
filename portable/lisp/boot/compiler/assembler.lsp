;; *inst-table* - список инструкций для генерации байт-кода.
(defvar *inst-table*
  '(lda
    jmp jnt
    global-set global-get
    push pop drop local-get local-set
    call ret
    prim1 prim2 prim3))
;; *inst-jmp-table* - список номеров инструкций перехода.
(defvar *inst-jmp-table* '(jmp jnt call))

;; Превращает список инструкций с метками в байт-код.
;; program - список инструкций.
(defun assemble (program)
  (let* ((jmp-labels nil)
         (jmp-addrs (make-hash))
         (bytecode-len 0)
         (bytecode-res
          (foldl #'(lambda (res-pc inst)
                     (if (symbolp inst)
                         (progn
                           (set-hash jmp-addrs inst (++ (cdr res-pc)))
                           res-pc)
                         (let ((pc (++ (cdr res-pc))))
                           (when (contains *inst-jmp-table* (car inst))
                             (setq jmp-labels
                                   (append jmp-labels
                                           (list (cons pc (cadr inst))))))
                           (cons (append
                                  (car res-pc)
                                  (append
                                   (list (list-search *inst-table* (car inst)))
                                   (foldl #'(lambda (res elem)
                                              (setq pc (++ pc))
                                              (append res (list elem)))
                                          nil (cdr inst))))
                                 pc))))
                 (cons nil 0) program))
         (bytecode (car bytecode-res))
         (bytecode-len (cdr bytecode-res))
         (bytecode-arr (make-array bytecode-len)))
    (foldl #'(lambda (pc elem)
               (seta bytecode-arr pc elem)
               (++ pc))
           0 bytecode)
    (foldl #'(lambda (pc addr-label)
               (seta bytecode-arr
                     (car addr-label)
                     (- (get-hash jmp-addrs (cdr addr-label)) (car addr-label))))
           0 jmp-labels)
    bytecode-arr))

(defun contains (list elem)
  ;; (print (list 'contains list elem))
  (if (null list)
      nil
      (progn
        (when (atom list)
          (error "Not list in contains"))
        (if (eq (car list) elem)
            t
            (contains (cdr list) elem)))))
