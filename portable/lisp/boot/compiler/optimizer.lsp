(defvar *optimize-flags* ; список включенных флагов оптимизации промежуточного дерева
  '(trivial-condition
    simplify-arithmetic
    dead-code-elimination
    tail-call
    constant-folding))

(defconst +const-fix-prims+ ; список FIX-примитивов, которые можно посчитать заранее на этапе компиляции
  '(car cdr atom cons
    % << >> eq equal > < sin cos sqrt
    intern symbol-name string-size inttostr code-char char-code char subseq
    array-size aref symbolp integerp pairp functionp
    round ~ + - * / & bitor ^ arrayp stringp))
(defconst +const-nary-prims+ ; список NARY-примитивов, которые можно посчитать заранее на этапе компиляции
  '(+ - * / & bitor ^ concat))

(defun accumulator-loading (tree)
  "Удаляет избыточные формы загрузки значений в аккумулятор"
  (labels ((is-same-param (set ref)
             (let ((set-expr (if (eq (car set) 'DEEP-SET) (forth set) (third set))))
               (and (contains '(CONST GLOBAL-REF LOCAL-REF DEEP-REF FIX-CLOSURE PRIM-CLOSURE NPRIM-CLOSURE) (car set-expr))
                    (equal set-expr ref))))
           (is-set-ref (set ref)
             (and (not (null set))
                  (contains '(GLOBAL-SET LOCAL-SET DEEP-SET) (car set))
                  (or (and (or (and (eq (car set) 'GLOBAL-SET) (eq (car ref) 'GLOBAL-REF))
                               (and (eq (car set) 'LOCAL-SET) (eq (car ref) 'LOCAL-REF))
                               (and (eq (car set) 'DEEP-SET) (eq (car ref) 'DEEP-REF)))
                           (eq (second set) (second ref)))
                      (is-same-param set ref))))
           (remove-set-ref (prev-elem subtree)
             (if (null subtree)
                 nil
                 (let* ((cur-elem (car subtree))
                        (next-elem (remove-set-ref cur-elem (cdr subtree))))
                   (if (is-set-ref prev-elem cur-elem)
                       next-elem
                       (cons cur-elem next-elem))))))
    (cons 'SEQ (remove-set-ref nil (cdr tree)))))

(defun trivial-condition (alter)
  "Если условие формы ALTER - константа, то считает значение условия и сокращает форму ALTER до одной из её веток"
  (if (contains *optimize-flags* 'trivial-condition)
  (let ((cond (second alter))
        (true (third alter))
        (false (forth alter)))
    (if (or (eq (car cond) 'CONST)
            (and (eq (car cond) 'GLOBAL-REF)
                 (contains '(0 1) (cadr cond))))
        (let ((cond-val (case (car cond)
                          ('CONST (cadr cond))
                          ('GLOBAL-REF (case (cadr cond) (0 t) (1 nil) (otherwise nil)))
                          (otherwise nil))))
          (if cond-val true false))
        alter)) alter))

(defun simplify-arithmetic (tree)
  "Заменяет вызов арифметического примитива tree с неограниченным кол-вом аргументов на вложенные вызовы примитивов с фиксированным кол-вом аргументов"
  (if (contains *optimize-flags* 'simplify-arithmetic)
  (let ((prim (second tree))
        (args (forth tree)))
    (if (and (contains '(+ - * / & bitor ^) prim)
             (>= (list-length args) 2))
        (foldl #'(lambda (prev cur)
                   `(FIX-PRIM ,prim (,prev ,cur)))
               (car args) (cdr args))
        tree)) tree))

(defun constant-folding (tree)
  "Свёртка и распространение констант"
  (labels ((try-compute (tree)
             "Пытается вычислить значение промежуточной формы, если она константа"
             "Возвращает список, состоящий из одного элемента - вычисленного выражения, либо nil"
             (case (car tree)
               ('CONST (list (second tree)))
               ('GLOBAL-REF (case (second tree) (0 (list t)) (1 (list nil)) (otherwise nil)))
               (otherwise
                (when (or (and (eq (car tree) 'FIX-PRIM) (contains +const-fix-prims+ (second tree)))
                          (and (eq (car tree) 'NARY-PRIM) (contains +const-nary-prims+ (second tree))))
                  (let ((prim (second tree))
                        (vals (map #'try-compute (if (eq (car tree) 'FIX-PRIM) (third tree) (forth tree)))))
                    (unless (contains vals nil)
                      (list (apply (symbol-function prim) (map #'car vals))))))))))
    (if (contains *optimize-flags* 'constant-folding)
        (let ((const-val (try-compute tree)))
          (if (null const-val)
              tree
              (list 'CONST (car const-val))))
        tree)))

(defun expand-seqs (tree)
  "Разворачивает вложенные SEQ"
  "tree - SEQ-форма"
  (labels ((expand (subtree)
             (if (null subtree)
                 nil
                 (let ((cur-elem (car subtree))
                       (next-elems (expand (cdr subtree))))
                   (if (eq (car cur-elem) 'SEQ)
                       (append (expand (cdr cur-elem)) next-elems)
                       (cons cur-elem next-elems))))))
    (cons 'SEQ (expand (cdr tree)))))

(defun dead-code-elimination (tree)
  "Убирает из формы SEQ неиспользуемый код"
  "К неиспользованнму коду относится:"
  " - последовательность команд загрузки аккумулятора (кроме последней команды)"
  (labels ((remove-acc-loading (subtree)
             (let ((cur-elem (car subtree))
                   (rest-elems (cdr subtree)))
               (cond ((null rest-elems) (list cur-elem))
                     ((and (not (eq (car (second subtree)) 'RETURN))
                           (contains '(CONST GLOBAL-REF LOCAL-REF DEEP-REF) (car cur-elem)))
                      (remove-acc-loading rest-elems))
                     (t (cons cur-elem (remove-acc-loading rest-elems)))))))
    (if (contains *optimize-flags* 'dead-code-elimination)
        (accumulator-loading (cons 'SEQ (remove-acc-loading (cdr (expand-seqs tree)))))
        tree)))

(defun tail-call (tree)
  "Оптимизация хвостовой рекурсии"
  "tree - форма TAIL-CALL или TAIL-NCALL"
  (if (contains *optimize-flags* 'tail-call)
      tree
      (cons (if (eq (car tree) 'TAIL-CALL) 'FIX-CALL 'NARY-CALL) (cdr tree))))

(defun optimize-tree (tree)
  "Оптимизация промежуточной формы tree методами, указанными флагами flags"
  "Возвращает оптимизированное дерево"
  (labels ((optimize-many (tree) (map #'optimize tree))
	   (optimize-cdr (tree) (cons (car tree) (optimize-many (cdr tree))))
	   (optimize-nth (tree n) (if (= n 1)
				      (cons (optimize (car tree)) (cdr tree))
				      (cons (car tree) (optimize-nth (cdr tree) (-- n)))))
	   (optimize (tree)
	     (if (contains '(NOP CONST GLOBAL-REF LOCAL-REF DEEP-REF RETURN PRIM-CLOSURE NPRIM-CLOSURE GOTO) (car tree))
		 tree
		 (case (car tree)
		   ('SEQ (dead-code-elimination (optimize-cdr tree)))
		   ('ALTER (trivial-condition (optimize-cdr tree)))
		   ('NARY-PRIM (constant-folding (simplify-arithmetic
				(list (car tree) (second tree) (third tree) (optimize-many (forth tree))))))
		   ('LABEL (if (null (cddr tree)) tree (list (car tree) (second tree) (optimize (third tree)))))
		   ('FIX-CLOSURE (if (null (forth tree)) tree (optimize-nth tree 4)))
		   ('GLOBAL-SET (optimize-nth tree 3))
		   ('LOCAL-SET (optimize-nth tree 3))
		   ('DEEP-SET (optimize-nth tree 4))
		   ('FIX-LET (list (car tree) (second tree) (optimize-many (third tree)) (optimize (forth tree))))
		   ('FIX-PRIM (constant-folding (list (car tree) (second tree) (optimize-many (third tree)))))
		   ('FIX-CALL (list (car tree) (second tree) (third tree) (optimize-many (forth tree))))
		   ('TAIL-CALL (tail-call (list (car tree) (second tree) (third tree) (optimize-many (forth tree)) (fifth tree))))
		   ('NARY-CALL (list (car tree) (second tree) (third tree) (forth tree) (optimize-many (fifth tree))))
		   ('TAIL-NCALL (tail-call (list (car tree) (second tree) (third tree) (forth tree) (optimize-many (fifth tree)) (sixth tree))))
		   ('CATCH (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   ('THROW (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   (otherwise (comp-err "optimize-tree: invalid expression" tree))))))
    (optimize tree)))
