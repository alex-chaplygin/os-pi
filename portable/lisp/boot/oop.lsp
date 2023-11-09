(defmacro defclass (name parent slots)
  "Создание нового класса
  name - имя, parent - родительский класс,
  slots - список полей"
  "(defclass point () (x y))"
  nil)

(defmacro make-instance (class)
  "Создать экземпляр объекта класса class"
  nil)

(defun make-hash () (nil))

(defun hash-add (hash key val)
  (cond
    ((null (car hash)) (rplaca hash (cons key (cons val nil))))
    ((eq key (caar hash)) (rplaca (cdar hash) val))
    (t (hash-add 
