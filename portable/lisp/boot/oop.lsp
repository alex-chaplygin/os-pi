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
  `(defun ,(intern (concat "MAKE-" (symbol-name name))) ,(get-slots name)
     (let ((obj (make-instance ,name)))
       (progn
	 nil)));,@(map '(lambda(s) `(setf (slot obj ,s) ,s)) (get-slots name)))))
	 

  `(let ((class (make-hash)))
	 (progn
	   (set-hash class 'parent ',parent)
	   (set-hash class 'slots ',slots)
	   (set-hash *class-table* ',name class)
	   ',name)))
	
	
(defmacro make-instance (class)
  "Создать экземпляр объекта класса class"
  "(make-instance 'point) -> ((X.nil)(Y.nil))"
  (if (not (check-key *class-table* class)) (concat "no class " (symbol-name class))
    `(let ((object (make-hash)))
       (progn
	 (set-hash object 'class ',class)
	 (app '(lambda (x) (set-hash object x nil)) ',(get-slots class))
	 object))))


(defun get-slots (class)
  "Получение списка свойств класса class"
  (if (null class)  nil
    (let* ((cl (get-hash *class-table* class))
	   (slots (get-hash cl 'slots))
	   (parent (get-hash cl 'parent)))
      (append (get-slots parent) slots))))

(defmacro slot (obj key)
  "Возвращает значение свойства key у объекта obj"
  `(get-hash ,obj ',key))
  
(defclass point () (x y))

(defclass line point (x2 y2))

*class-table*

;(defun make-point (x y) 

;(let ((obj (make-instance point)))
;(setf (slot obj x) x)
;(setf (slot obj y) y)
;obj)
(slot p1 x)
  

(setq p2 (make-point 10 20))

(setq l1 (make-instance line))
;(move p1 20 20)
(map '(lambda (x) (* x x)) '(1 2 3))

(map '(lambda(s) `(setf(slot obj ,s) ,s)) '(x y z))
