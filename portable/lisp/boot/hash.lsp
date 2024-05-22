(defun make-hash ()
  "Создание пустой хэш-таблицы (hash))"
  (list 'hash))

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
  (set-hash* hash key val)
  val)

(defun get-hash* (hash key)
  "Возвращает значение по ключу key из таблицы hash"
  (when (null hash) (error (concat "get-hash: no key " (symbol-name key))))
  (if (equal key (caar hash)) (cdar hash)
      (get-hash* (cdr hash) key)))
(defun get-hash (hash key)
  (when (empty-hash hash) (error "get-hash: empty hash"))
  (get-hash* (cdr hash) key))

(defun check-key* (hash key)
  "Проверяет есть ли ключ key в таблице hash"
  (if (or (null hash) (null (car hash))) nil
    (if (equal key (caar hash))
	t
      (check-key* (cdr hash) key))))
(defun check-key (hash key)
  (if (empty-hash hash) nil (check-key* (cdr hash) key)))

(defun remove-key (hash key)
  "Удаляет ключ key в таблице hash, без проверки вхождения"
  (when (empty-hash hash) (error "remove-key: empty hash"))
  (remove-key* hash (cdr hash) key))
(defun remove-key* (prev hash key)
  (when (null hash) (error (concat "remove-key: no key " (symbol-name key))))
  (if (equal key (caar hash)) (rplacd prev (cdr hash))
      (remove-key* (cdr prev) (cdr hash) key)))

(defun hash-test ()
  (setq a (make-hash))
  (setq b (make-hash))
  (set-hash a 'x 5)
  (set-hash a 'y 10)
  (set-hash a 'x 25)
  (set-hash a 'z 35)
  (set-hash b 'test 'alpha)
  (set-hash a 'test b)
  (get-hash a 'x)
  (check-key a 'z)
;  (remove-key a 'w)
  a)
