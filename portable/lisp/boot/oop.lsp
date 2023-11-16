					; Таблица классов хранит имя класса и список свойств
					; ((point.(X Y)))
					; ((line . (x y x2 y2)))
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
  (set-hash *class-table* name (make-slots parent slots)))

(defmacro make-instance (class)
  "Создать экземпляр объекта класса class"
  "(make-instance point) -> ((X.nil)(Y.nil))"
  nil)

(defun make-slots (parent slots)
  "Получение списка свойств"
  (if (null parent) slots
    (append slots (get-hash *class-table* parent))))
