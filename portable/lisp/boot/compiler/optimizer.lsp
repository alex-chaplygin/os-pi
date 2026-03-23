(defvar *optimize-flags* ; список включенных флагов оптимизации промежуточного дерева
  '(trivial-condition
    simplify-arithmetic
    dead-code-elimination
    tail-call
    constant-folding
    unused-functions
    beta-expansion))

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

(defun unused-functions (tree)
  "Удаление неиспользуемых функций"
  "tree - LABEL-форма"
  (if (contains *optimize-flags* 'unused-functions)
      (if (and (check-key *functions-info* (second tree))
	       (= (get-hash (get-hash *functions-info* (second tree)) 'count) 0))
	  (list 'NOP) tree)
      tree))

(defun no-local-set (exp)
  "В выражении отсутствует LOCAL-SET или FIX-LET"
  (cond ((null exp) t)
	((atom exp) t)
	((and (pairp exp) (atom (car exp)) (or (eq (car exp) 'LOCAL-SET) (eq (car exp) 'FIX-LET) (eq (car exp) 'FIX-CLOSURE))) nil)
	(t (and (no-local-set (car exp)) (no-local-set (cdr exp))))))

(defun one-ref-args (body)
  "К каждому аргументу только одно обращение"
  (let ((counts (make-hash)))
    (labels ((refs (exp)
	       (cond ((null exp) t)
		     ((atom exp) t)
		     ((and (pairp exp) (atom (car exp)) (eq (car exp) 'LOCAL-REF))
		      (if (check-key counts (second exp)) nil (set-hash counts (second exp) t)))
		     (t (and (refs (car exp)) (refs (cdr exp)))))))
      (refs body))))

(defun beta-exp (exp args)
  "Выполнить подстановку тела функции"
  (cond ((null exp) nil)
	((atom exp) exp)
	((and (pairp exp) (atom (car exp)))
	 (case (car exp)
	   ('LOCAL-REF (nth args (second exp)))
	   ('DEEP-REF (let ((lev (-- (second exp))))
			(if (= lev 0) (list 'LOCAL-REF (third exp)) (list 'DEEP-REF lev (third exp)))))
	   ('DEEP-SET (list 'DEEP-SET (-- (second exp)) (third exp) (forth exp)))
	   ('RETURN (list 'NOP))
	   (otherwise (cons (beta-exp (car exp) args) (beta-exp (cdr exp) args)))))
	(t (cons (beta-exp (car exp) args) (beta-exp (cdr exp) args)))))

(defun beta-expansion (call)
  "Подстановка тела функции, когда функция вызывается один раз и это возможно"
  (if (contains *optimize-flags* 'beta-expansion)
      (let* ((f (get-hash *functions-info* (second call)))
  	     (count (get-hash f 'count))
	     (rec (check-key f 'rec))
  	     (body (if rec nil (get-hash f 'body))))
  	(if (and (= count 1) (not rec)
  		 (= (case (car call) ('FIX-CALL (third call)) ('NARY-CALL (forth call))) 0)
		 (no-local-set body) (one-ref-args body)) ;;(no-side-effects args))
	    (progn ;(print `(beta-exp ,call))
		   (beta-exp body (last call))) call))
      call))

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
		   ('LABEL (let ((body (if (null (cddr tree)) nil (optimize (third tree)))))
			     (when (check-key *functions-info* (second tree))
			       (set-hash (get-hash *functions-info* (second tree)) 'body body))
			     (unused-functions (if (null (cddr tree)) tree (list (car tree) (second tree) body)))))
		   ('FIX-CLOSURE (if (null (forth tree)) tree (optimize-nth tree 4)))
		   ('GLOBAL-SET (optimize-nth tree 3))
		   ('LOCAL-SET (optimize-nth tree 3))
		   ('DEEP-SET (optimize-nth tree 4))
		   ('FIX-LET (list (car tree) (second tree) (optimize-many (third tree)) (optimize (forth tree))))
		   ('FIX-PRIM (constant-folding (list (car tree) (second tree) (optimize-many (third tree)))))
		   ('FIX-CALL (beta-expansion (list (car tree) (second tree) (third tree) (optimize-many (forth tree)))))
		   ('TAIL-CALL (tail-call (list (car tree) (second tree) (third tree) (optimize-many (forth tree)) (fifth tree))))
		   ('NARY-CALL (beta-expansion (list (car tree) (second tree) (third tree) (forth tree) (optimize-many (fifth tree)))))
		   ('TAIL-NCALL (tail-call (list (car tree) (second tree) (third tree) (forth tree) (optimize-many (fifth tree)) (sixth tree))))
		   ('CATCH (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   ('THROW (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   (otherwise (comp-err "optimize-tree: invalid expression" tree))))))
    (optimize tree)))
