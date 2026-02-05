(unit-tests 'optimizer)

;; (defvar *optimizer-test-counter* 0) ; счётчик объявления тестов оптимизаций для deftest

;; (defun test-optimize (expr phase-stop flags expected-res)
;;   "Тест-кейс оптимизатора для S-выражения expr, который может остановиться на стадии статического анализа или генератора (phase-stop), применяет оптимизации flags и ожидает expected-res"
;;   "Возвращает результат теста (список с OK или FAIL) или сообщение об ошибке компилятора"
;;   (let ((tree nil)
;;         (tree-optimized nil)
;;         (assembly nil)
;;         (assembly-optimized nil))
;;     (print "Результат теста:"
;;            (assert
;;             (catch 'compiler
;;               (print "Исходное S-выражение:" expr)
;;               (print "Промежуточное дерево:" (setq tree (compile expr)))
;;               (setq tree-optimized (optimize-tree tree flags))
;;               (when (not (equal tree tree-optimized))
;;                 (print "Оптимизированное дерево:" tree-optimized))
;;               (case phase-stop
;;                 ('compile tree-optimized)
;;                 ('generate
;;                  (progn
;;                    (print "Код ассемблера:" (setq assembly (generate tree-optimized)))
;;                    (setq assembly-optimized (optimize-assembly assembly flags))
;;                    (when (not (equal assembly assembly-optimized))
;;                      (print "Оптимизированный код:" assembly-optimized))
;;                    assembly-optimized))
;;                 (otherwise (unreachable))))
;;             expected-res))
;;     (print)))

;; (defmacro optimizer-tests (phase-stop flags tests)
;;   "Объявляет через deftest набор тестов оптимизации tests, состоящих из документации теста, исходного выражения и ожидаемого результата, с флагами flags и сравнивающихся на стадии компилятора phase-stop"
;;   (cons 'progn
;;         (map #'(lambda (test)
;;                  (let ((doc (car test))
;;                        (expr (second test))
;;                        (expected-res (third test)))
;;                    (incf *optimizer-test-counter*)
;;                    `(deftest ,(intern (concat "OPTIMIZER-TEST-" (inttostr *optimizer-test-counter*)))
;;                         () ,doc (test-optimize ,expr ,phase-stop ,flags ,expected-res))))
;;              `,tests)))

;; (optimizer-tests
;;  'generate '(accumulator-loading)
;;  (("Тест простого случая подряд идущих команд загрузки в аккумулятор"
;;    '(progn 1 2 3)
;;    '((CONST 3))) ; 3
;;   ("Тест последовательности команд загрузки в аккумулятор вперемешку с setq"
;;    '(progn 1 (setq a 5) 2 3 a 4 5)
;;    '((CONST 5) (GLOBAL-SET 2) (CONST 5))) ; (progn (setq a 5) 5)
;;   ("Тест подряд идущих команд загрузки в аккумулятор внутри if"
;;    '(if t (progn 1 2 3) nil)
;;    '((GLOBAL-REF 0) (JNT G1) (CONST 3) (JMP G2) (LABEL G1) (GLOBAL-REF 1) (LABEL G2))) ; (if t 3 nil)
;;   ("Тест удаления REF-команды после аналогичной SET-команды той же переменной"
;;    '(progn (setq a 5) a)
;;    '((CONST 5) (GLOBAL-SET 2))) ; (setq a 5)
;;   ("Тест того, что оптимизация не удалит обращение к константе сразу после присваивания переменной"
;;    '(progn (setq a 5) 10)
;;    '((CONST 5) (GLOBAL-SET 2) (CONST 10))) ; (progn (setq a 5) 10)
;;   ("Тест того, что оптимизация не удалит обращение к переменной сразу после присваивания другой переменной"
;;    '(progn (setq a 5) (setq b 10) a)
;;    '((CONST 5) (GLOBAL-SET 2) (CONST 10) (GLOBAL-SET 3) (GLOBAL-REF 2))) ; (progn (setq a 5) (setq b 10) a)
;;   ("Комбинированный тест удаления команд загрузки в аккумулятор"
;;    '(progn 1 2 (setq a 3) a 4 5)
;;    '((CONST 3) (GLOBAL-SET 2) (CONST 5))))) ; (progn (setq a 3) 5)

;; (optimizer-tests
;;  'compile '(trivial-condition)
;;  (("Тест простого случая оптимизации условного оператора if с константным условием (специальный символ t)"
;;    '(if t 1 2)
;;    '(CONST 1)) ; 1
;;   ("Тест простого случая оптимизации условного оператора if с константным условием (специальный символ nil)"
;;    '(if nil 1 2)
;;    '(CONST 2)) ; 2
;;   ("Тест простого случая оптимизации условного оператора if с константным условием (константа)"
;;    '(if 1 1 2)
;;    '(CONST 1)))) ; 1

;; (optimizer-tests
;;  'compile '(simplify-arithmetic)
;;  (("Тест оптимизации арифметических примитивов от меньше чем 2-х аргументов"
;;    '(- 10)
;;    '(NARY-PRIM - 1 ((CONST 10)))) ; (- 1)
;;   ("Тест оптимизации арифметических примитивов от 2-х аргументов"
;;    '(+ 1 2)
;;    '(FIX-PRIM + ((CONST 1) (CONST 2)))) ; (+ 1 2)
;;   ("Тест оптимизации арифметических примитивов от больше чем 2-х аргументов"
;;    '(+ 1 2 3 4)
;;    '(FIX-PRIM + ((FIX-PRIM + ((FIX-PRIM + ((CONST 1) (CONST 2))) (CONST 3))) (CONST 4)))))) ; (+ (+ (+ 1 2) 3) 4)

;; (optimizer-tests
;;  'compile '(dead-code-elimination)
;;  (("Тест оптимизации удаления комментариев функции"
;;    '(defun null (x)
;;      "Проверка на пустое значение"
;;      (eq x ()))
;;    '(LABEL NULL (SEQ (SEQ (FIX-PRIM EQ ((LOCAL-REF 0) (CONST ())))) (RETURN)))))) ; (defun null (x) (eq x ()))

;; (optimizer-tests
;;  'compile '(tail-call)
;;  (("Простой тест оптимизации хвостовой рекурсии"
;;    '(defun f (x) (f x))
;;    '(LABEL F (SEQ (LABEL BEGIN) (SEQ (LOCAL-SET 0 (LOCAL-REF 0)) (GOTO BEGIN)) (RETURN))))
;;   ("Тест факториала, реализованного через хвостовую рекурсию"
;;    '(defun fact (acc x)
;;      (if (equal x 0)
;;          acc
;;          (fact (* acc x) (- x 1))))
;;    '(LABEL FACT
;;      (SEQ
;;       (LABEL BEGIN)
;;       (ALTER (FIX-PRIM EQUAL ((LOCAL-REF 1) (CONST 0)))
;;        (LOCAL-REF 0)
;;        (SEQ
;;         (LOCAL-SET 0 (NARY-PRIM * 0 ((LOCAL-REF 0) (LOCAL-REF 1))))
;;         (LOCAL-SET 1 (NARY-PRIM - 1 ((LOCAL-REF 1) (CONST 1))))
;;         (GOTO BEGIN)))
;;       (RETURN))))
;;   ("Сложный тест оптимизации хвостовой рекурсии"
;;    '(defun f (x)
;;      (catch 'f
;;        ((lambda (a b)
;;           (f (+ a b)))
;;         1 2)))
;;    '(LABEL F (SEQ (LABEL BEGIN)
;;               (CATCH (CONST F)
;;                 (FIX-LET 2 ((CONST 1) (CONST 2))
;;                          (SEQ (DEEP-SET 1 0 (NARY-PRIM + 0 ((LOCAL-REF 0) (LOCAL-REF 1))))
;;                               (GOTO BEGIN))))
;;               (RETURN))))
;;   ("Простой тест того, что оптимизация не тронет обычную функцию"
;;    '(defun f (x) x)
;;    '(LABEL F (SEQ (LOCAL-REF 0) (RETURN))))
;;   ("Сложный тест того, что оптимизация не тронет обычную функцию"
;;    '(defun f (x)
;;      (catch 'f
;;        ((lambda (a b)
;;           (+ a b))
;;         1 2)))
;;    '(LABEL F (SEQ (CATCH (CONST F)
;;                     (FIX-LET 2 ((CONST 1) (CONST 2))
;;                              (NARY-PRIM + 0 ((LOCAL-REF 0) (LOCAL-REF 1)))))
;;               (RETURN))))))

;; TODO: тесты для разных комбинаций оптимизаций, в том числе для комбинации всех оптимизаций

(deftest triv-cond-test ()
  (print (assert (trivial-condition '(ALTER (CONST T) (CONST 1) (CONST 2))) '(CONST 1)))
  (print (assert (trivial-condition '(ALTER (CONST ()) (CONST 3) (CONST 4))) '(CONST 4)))
  (print (assert (trivial-condition '(ALTER (GLOBAL-REF 0) (CONST 5) (CONST 6))) '(CONST 5)))
  (print (assert (trivial-condition '(ALTER (GLOBAL-REF 1) (CONST 7) (CONST 8))) '(CONST 8)))
  (print (assert (trivial-condition '(ALTER (FIX-PRIM EQ ((LOCAL-REF 0) (GLOBAL-REF 1))) (LOCAL-REF 1) (LOCAL-REF 2))) '(ALTER (FIX-PRIM EQ ((LOCAL-REF 0) (GLOBAL-REF 1))) (LOCAL-REF 1) (LOCAL-REF 2))))
  (print (assert (optimize-tree '(SEQ (ALTER (CONST T) (CONST 1) (CONST 2))
				  (ALTER (CONST NIL) (CONST 3) (CONST 4)))) '(SEQ (CONST 1) (CONST 3))))
  )

(deftest arith-test ()
  (print (assert (optimize-tree '(NARY-PRIM + 0 ((LOCAL-REF 0) (LOCAL-REF 1)))) ()))
  (print (assert (optimize-tree '(SEQ (LABEL APPEND2 (SEQ (ALTER (FIX-PRIM EQ ((LOCAL-REF 0) (GLOBAL-REF 1))) (LOCAL-REF 1) (FIX-PRIM CONS ((FIX-PRIM CAR ((LOCAL-REF 0))) (FIX-CALL APPEND2 0 ((FIX-PRIM CDR ((LOCAL-REF 0))) (LOCAL-REF 1)))))) (RETURN))))) ()))
  (print (assert (optimize-tree '(LABEL O (SEQ (FIX-CLOSURE G2 1 (LABEL G2 (SEQ (NARY-PRIM FUNCALL 1 ((DEEP-REF 1 0) (NARY-PRIM FUNCALL 1 ((DEEP-REF 1 1) (LOCAL-REF 0))))) (RETURN)))) (RETURN)))) ()))
  (print (assert (optimize-tree '(LABEL HASH-TEST (SEQ (SEQ (GLOBAL-SET 2 (FIX-CALL MAKE-HASH 0 ())) (GLOBAL-SET 3 (FIX-CALL MAKE-HASH 0 ())) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 2) (CONST X) (CONST 5))) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 2) (CONST Y) (CONST 10))) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 2) (CONST X) (CONST 25))) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 2) (CONST Z) (CONST 35))) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 3) (CONST TEST) (CONST ALPHA))) (FIX-CALL SET-HASH 0 ((GLOBAL-REF 2) (CONST TEST) (GLOBAL-REF 3))) (FIX-CALL GET-HASH 0 ((GLOBAL-REF 2) (CONST X))) (FIX-CALL CHECK-KEY 0 ((GLOBAL-REF 2) (CONST Z))) (GLOBAL-REF 2)) (RETURN)))) ()))
  )

				  
(run-tests 'stop-on-fail)
