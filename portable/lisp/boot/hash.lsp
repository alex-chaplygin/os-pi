(defun make-hash ()
  "Создание пустой хэш-таблицы (hash))"
  '(hash))

(defun empty-hash (hash)
  "Проверяет hash на пустоту"
  (null (cdr hash)))

(defun set-hash* (hash key val)
  "Добавление или изменение значения val по ключу key в таблице hash"
  "(hash (key.val) (key2.val2))"
  (let ((em (empty-hash hash)))
    (if (or em (equal key (caadr hash)))
	(rplacd hash (cons (cons key val) (if em nil (cddr hash))))
	(set-hash* (cdr hash) key val))))
(defun set-hash (hash key val)
  (if (empty-hash hash) (rplacd hash (cons (cons key val) nil))
      (set-hash* hash key val))
  val)

(defun get-hash (hash key)
  "Возвращает значение по ключу key из таблицы hash"
  (if (or (null hash) (null (car hash))) (concat "no key " (symbol-name key))
    (if (equal key (caar hash))
	(cdar hash)
      (get-hash (cdr hash) key))))

(defun check-key (hash key)
  "Проверяет есть ли ключ key в таблице hash"
  (if (or (null hash) (null (car hash))) nil
    (if (equal key (caar hash))
	t
      (check-key (cdr hash) key))))

(defun remove-key (hash key)
  "Удаляет ключ key в таблице hash, без проверки вхождения"
  (if (or (null hash) (null (car hash))) nil
      (if (equal key (caar hash)))))

(defun hash-test ()
  (setq a (make-hash))
  (set-hash a 'x 5)
  (set-hash a 'y 10)
  (set-hash a 'x 25)
  a)

(hash-test)
