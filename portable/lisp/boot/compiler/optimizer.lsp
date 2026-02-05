(defvar *optimize-flags* ; список всех существующих флагов оптимизации промежуточного дерева
  '(trivial-condition
    simplify-arithmetic
    dead-code-elimination))

;; (defun optimize-accumulator-loading (assembly)
;;   "Удаляет избыточные формы загрузки значений в аккумулятор в коде ассемблера assembly"
;;   (labels ((is-loading (inst) (contains '(CONST GLOBAL-REF LOCAL-REF DEEP-REF) (car inst)))
;;            (is-set-ref (set-inst ref-inst)
;;              (and (not (eq (car ref-inst) 'CONST))
;;                   (not (null set-inst))
;;                   (or (and (eq (car set-inst) 'GLOBAL-SET) (eq (car ref-inst) 'GLOBAL-REF))
;;                       (and (eq (car set-inst) 'LOCAL-SET) (eq (car ref-inst) 'LOCAL-REF))
;;                       (and (eq (car set-inst) 'DEEP-SET) (eq (car ref-inst) 'DEEP-REF)))
;;                   (eq (cadr set-inst) (cadr ref-inst))))
;;            (take-while (list pred acc)
;;              (cond ((or (null list) (not (funcall pred (car list)))) (cons acc list))
;;                    (t (take-while (cdr list) pred (append acc (list (car list)))))))
;;            (compress-ref (assembly)
;;              (if (null assembly)
;;                  nil
;;                  (let* ((pair (take-while assembly #'is-loading nil))
;;                         (load-inst-list (car pair))
;;                         (load-inst (if (null load-inst-list) nil (last load-inst-list)))
;;                         (rest-inst-list (cdr pair)))
;;                    (if (null load-inst)
;;                        (cons (car rest-inst-list) (compress-ref (cdr rest-inst-list)))
;;                        (cons load-inst (compress-ref rest-inst-list))))))
;;            (simplify-set-ref (assembly prev-inst)
;;              (cond ((null assembly) nil)
;;                    ((is-set-ref prev-inst (car assembly)) (simplify-set-ref (cdr assembly) (car assembly)))
;;                    (t (cons (car assembly) (simplify-set-ref (cdr assembly) (car assembly)))))))
;;     (simplify-set-ref (compress-ref assembly) nil)))

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

;; (defun optimize-constant-folding (tree) ;; TODO: не использовано
;;   "Пытается вычислить значение промежуточной формы, если она константа"
;;   "Возвращает список, состоящий из одного элемента - вычисленного выражения, либо nil"
;;   (case (car tree)
;;     ('CONST (list (cadr tree)))
;;     ('GLOBAL-REF (list (case (cadr tree) (0 t) (1 nil))))
;;     (otherwise
;;      (when (contains '(FIX-PRIM NARY-PRIM) (car tree))
;;        (let ((prim (cadr tree))
;;              (vals (map #'optimize-constant-folding
;;                         (funcall
;;                          (case (car tree)
;;                            ('FIX-PRIM #'third)
;;                            ('NARY-PRIM #'forth)
;;                            (otherwise (unreachable)))
;;                          tree))))
;;          (unless (contains vals nil)
;;            (list (apply (symbol-function prim) (map #'car vals)))))))))

(defun dead-code-elimination (tree)
  "Убирает из формы SEQ неиспользуемый код"
  "К неиспользованнму коду относится:"
  " - последовательность команд загрузки аккумулятора (кроме последней команды)"
  tree)
  ;; (labels ((eliminate-dead-code (tree res)
  ;;            (let* ((subtree (car tree))
  ;;                   (res-added (append res (list subtree))))
  ;;              (if (null (cdr tree))
  ;;                  res-added
  ;;                  (eliminate-dead-code (cdr tree) (if (eq (car subtree) 'CONST) res res-added))))))
  ;;   (eliminate-dead-code (cdr tree) (list (car tree)))))

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
		   ('NARY-PRIM (simplify-arithmetic
				(list (car tree) (second tree) (third tree) (optimize-many (forth tree)))))
		   ('LABEL (if (null (cddr tree)) tree (list (car tree) (second tree) (optimize (third tree)))))
		   ('FIX-CLOSURE (if (null (forth tree)) tree (optimize-nth tree 4)))
		   ('GLOBAL-SET (optimize-nth tree 3))
		   ('LOCAL-SET (optimize-nth tree 3))
		   ('DEEP-SET (optimize-nth tree 4))
		   ('FIX-LET (list (car tree) (second tree) (optimize-many (third tree)) (optimize (forth tree))))
		   ('FIX-PRIM (list (car tree) (second tree) (optimize-many (third tree))))
		   ('FIX-CALL (list (car tree) (second tree) (third tree) (optimize-many (forth tree))))
		   ('NARY-CALL (list (car tree) (second tree) (third tree) (forth tree) (optimize-many (fifth tree))))
		   ('CATCH (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   ('THROW (list (car tree) (optimize (second tree)) (optimize (third tree))))
		   (otherwise (comp-err "optimize-tree: invalid expresion" tree))))))
    (optimize tree)))
