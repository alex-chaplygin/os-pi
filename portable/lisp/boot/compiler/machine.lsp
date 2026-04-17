;; генерация машинного кода для ассемблера nasm
;; вычисляем и запоминаем константы
;; подставляем номера констант
;; подставляем номера примитивов
;; добавляем выравнивание для функций с нечетным числом аргументов

(setq reverse-args t)

(defun machine-const (inst)
  "Подставляем номер константы в инструкцию, запоминаем константы"
  (let* ((c (cadr inst))
	 (lc (list c))
         (i (list-search *consts* c)))
    (when (null i)
      (setq i (++ (list-length *consts*)))
      (rplacd *consts-end* lc)
      (setq *consts-end* lc)
      (decf i))
    (list 'CONST i)))

(defun machine-prim (inst)
  "Подставляем номер примитива в инструкцию"
  (let* ((prim-type (car inst))
         (lst (case prim-type
                ('prim *fix-primitives*)
                ('prim-closure *fix-primitives*)
                ('nprim *nary-primitives*)
                ('nprim-closure *nary-primitives*)))
         (prim (search-symbol lst (cadr inst)))
         (prim-i (list-search lst prim)))
    (case prim-type
      ('nprim (if (eq (second inst) 'funcall) (list 'APPLY) (list prim-type prim-i)))
      ('prim (case (second inst)
	       ('apply (list 'APPLY))
	       ('+ (list 'ADD))
	       ('* (list 'MUL))
	       ('< (list 'LESS))
	       ('eq (list 'EQ))
	       (otherwise (list prim-type prim-i))))
      (otherwise (list prim-type prim-i)))))

(defun assemble-machine (gen)
  "Преобразование списка инструкций виртуальной машины в список инструкций для ассемблера nasm"
  (setq *consts* (list t nil) *consts-end* (cdr *consts*))
  (map #'(lambda (i)
	   (cond
	       ((contains '(prim nprim prim-closure nprim-closure) (car i)) (machine-prim i))
	       (t (case (car i)
		    ('const (machine-const i))
		    (otherwise i)))))  gen))
