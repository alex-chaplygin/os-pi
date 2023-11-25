					; Таблица классов хранит имя класса и список свойств
					; {
					;    point: {
					;      parent: nil,
					;      slots: (x y),
					;      move: (lambda (self dx dy) (setf (slot self x) (+ (slot self x) dx)
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
	   ',name))
  `(defun ,(intern (concat "MAKE-" (symbol-name name))) ,(get-slots name)
     (let ((obj (make-instance ,name)))
       (progn
	 ,@(map '(lambda(s) `(setf (slot obj ,s) ,s)) (get-slots name))
	 obj)))
  `',name)

	
	
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
  `(get-hash ,obj ,key))

(defun get-method (class-name method-name)
  "Рекурсивно возвращает тело метода method-name из класса class-name"
  (if (null class-name) (concat "no method " (symbol-name method-name))
    (let ((class (slot *class-table* class-name)))
      (if (check-key class method-name)
	  (slot class method-name)
	(get-method (slot class 'parent) method-name)))))

(defmacro defmethod (name args body)
    
(defclass point () (x y))

(defclass line point (x2 y2))

*class-table*
  
(macroexpand '(defclass point () (x y)))
(setq p2 (make-point 10 20))
(setq l1 (make-line 1 1 3 3))
;(move p1 20 20)

;(map '(lambda(s) `(setf(slot obj ,s) ,s)) '(x y z))
;(defmethod move ((self point) dx dy)
 ; (setf (slot self x) (+ (slot self x) dx))
  ;(setf (slot self y) (+ (slot self y) dy)))

;(defmethod move ((self line) dx dy)
 ; (setf (slot self x) (+ (slot self x) dx))
  ;(setf (slot self x2) (+ (slot self x2) dx))
 ; (setf (slot self y) (+ (slot self y) dy))
 ; (setf (slot self y2) (+ (slot self y2) dy)))

;(move l1 20 20)

(setq pc (slot *class-table* 'point))
(set-hash pc 'move '(lambda(x y) x))

(defun move (self dx dy)
  (funcall (get-method (slot self 'class) 'move) dx dy))

(get-method 'line 'move)
(get-method 'point 'move)
(get-method 'line 'move)
(move l1 'a 'b)
(move p2 'b 'c)
