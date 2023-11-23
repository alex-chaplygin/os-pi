(defun make-hash ()
  "Создание пустой хэш-таблицы (nil)"
  (cons nil nil))

(defun set-hash* (hash key val)
  "Добавление или изменение значения val по ключу key в таблице hash"
  "((key.val) (key2.val2))"
  (cond
   ((null hash) (cons (cons key val) nil))
   ((null (car hash)) (rplaca hash (cons key val)))
   (t (if (eq key (caar hash))
	  (progn
	    (rplacd (car hash) val)
	    hash)
	(rplacd hash (set-hash* (cdr hash) key val))))))
(defun set-hash (hash key val)
  (set-hash* hash key val)
  val)

(defun get-hash (hash key)
  "Возвращает значение по ключу key из таблицы hash"
  (if (or (null hash) (null (car hash))) (concat "no key " (symbol-name key))
    (if (eq key (caar hash))
	(cdar hash)
      (get-hash (cdr hash) key))))

(defun check-key (hash key)
  "Проверяет есть ли ключ key в таблице hash"
  (if (or (null hash) (null (car hash))) nil
    (if (eq key (caar hash))
	t
      (check-key (cdr hash) key))))
			
(defun hash-test ()
  (setq a (make-hash))
  (set-hash a 'x 5)
  (set-hash a 'y 10)
  a)
