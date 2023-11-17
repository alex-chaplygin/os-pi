					; Таблица классов хранит имя класса и список свойств
					; {
					;    point: {
					;      parent: nil,
					;      slots: (x y)
					;    },
					;    line: {
					;      parent: point,
					;      slots: (x2 y2)
					;    }
					; экземпляр класса point
					; ((class . point)(x.10)(y.20))
					;экземпляр класса line
					;((class . line)(x . 10)(y . 20)(x2 . 5)(y2 . 10))
(defvar *class-table* (make-hash))

(defmacro defclass (name parent slots)
  "Создание нового класса
  name - имя, parent - родительский класс,
  slots - список полей"
  "(defclass point () (x y))"
  `(let ((class (make-hash)))
	 (progn
	   (set-hash class 'parent ',parent)
	   (set-hash class 'slots ',slots)
	   (set-hash *class-table* ',name class)
	   ',name)))

(defmacro make-instance (class)
  "Создать экземпляр объекта класса class"
  "(make-instance 'point) -> ((X.nil)(Y.nil))"
  (if (not (check-key *class-table* ,class)) (concat "no class " (symbol-name class))
    `(let ((object (make-hash)))
       (progn
	 (set-hash object 'class ',class)  
	 nil))))


(defun get-slots (class)
  "Получение списка свойств класса class"
  (if (null class)  nil
    (let* ((cl (get-hash *class-table* class))
	   (slots (get-hash cl 'slots))
	   (parent (get-hash cl 'parent)))
      (append (get-slots parent) slots))))

(defclass point () (x y))

(defclass line point (x2 y2))

*class-table*

(get-slots 'point)
(get-slots 'line)
(funcall 'car '(1 2))
