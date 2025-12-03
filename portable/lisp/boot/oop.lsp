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

;; Максимальное количество классов
(defconst +max-class-count+ 100)

;; Индекс последнего созданного класса
(defvar *last-class* 0)

;; Индекс слотов в массиве класса
(defconst +slots-id+ 0)
;; Индекс родителя в массиве класса
(defconst +parent-id+ 1)
;; Индекс количества полей класса
(defconst +slots-count-id+ 2)
;; Поле номера класса в объекте
(defconst +class-id+ 0)
 
;; Массив классов
;; Класс - это массив массивов, len = 4
;; [0] - список полей класса
;; [1] - номер родителя
;; [2] - количество полей класса
;; [3] - список ??номеров методов 
(defvar *class-table* (make-array +max-class-count+))

;;Массив имен классов (имена хранятся в форме символов)
(defvar *class-names* (make-array +max-class-count+))

(defmacro gen-class-functions (class slots)
  "Генерация функций селекторов - <class>-<slot>"
  "мутаторов - <class>-set-<slot>"
  (if (null slots) nil
  `(progn
     (defun ,(intern (concat (symbol-name class) "-" (symbol-name (car slots)))) (obj)
       (slot obj ',(car slots)))
     (defun ,(intern (concat (symbol-name class) "-SET-" (symbol-name (car slots)))) (obj val)
       (setf (slot obj ',(car slots)) val))
     (gen-class-functions ,class ,(cdr slots)))))

(defun get-class-id (class-name)
  "Получение id класса"
  (if (null class-name) nil
    (labels ((search (index)
	       (if (= index *last-class*) nil
		 (if (eq class-name (aref *class-names* index)) index
		   (search (++ index))))))
	    (search 0))))


(defun get-slots-count(class-id)
  "Получение количества полей класса"
  (if (null class-id) 0
    (let ((class (aref *class-table* class-id)))
	(+ (list-length (aref class +slots-id+)) (get-slots-count (aref class +parent-id+))
	   ))))

(defun get-slots (class-id)
  "Получение списка свойств класса class"
  (if (null class-id) nil
    (let ((class (aref *class-table* class-id)))
      (if (null class) nil
	(append (get-slots (aref class +parent-id+)) (aref class +slots-id+))))
      ))

(defun make-instance (class-name)
  "Создать экземпляр объекта класса class"
  "(make-instance 'point) -> ((X.nil)(Y.nil))"
  ;; (let ((o (gensym)))
  ;;   `(if (not (check-key *class-table* ',class))
  ;; 	 (error (concat "no class " (symbol-name ',class)))
  ;; 	 (let ((,o (make-hash)))
  ;; 	   (set-hash ,o 'class ',class)
  ;; 	   (app #'(lambda (x) (set-hash ,o x nil)) ',(get-slots class))
  ;; 	   ,o))))
  (let* ((index (get-class-id class-name))
	 (class (aref *class-table* index))
	 (obj (make-array (++ (aref class +slots-count-id+))))
	 )
    (seta obj +class-id+ index)
    obj
    ))

(defmacro defclass (name parent slots)
  "Создание нового класса
  name - имя, parent - родительский класс,
  slots - список полей"
  "(defclass point (x y) ())"
  `(let* ((parent-id (get-class-id ',parent))
	  (class (make-array 3)))
     (seta class +slots-id+ ',slots)
     (seta class +parent-id+ parent-id)
     (seta class +slots-count-id+ (+ (get-slots-count parent-id) (list-length ',slots)))
     (seta *class-table* *last-class* class)
     (seta *class-names* *last-class* ',name))
  `(defun ,(intern (concat "MAKE-" (symbol-name name))) ,(get-slots *last-class*)
     ;; (let ((obj (make-instance ',name)))
     ;;   ,@(map #'(lambda(s) `(seta (slot obj ',s) ,s)) (get-slots *last-class*))
     ;;   obj)
     )
  ;; `(gen-class-functions ,name ,(get-slots *last-class*))
  `(incf *last-class*)
  `',name)

;; (defmacro slot (obj key)
;;   "Возвращает значение свойства key у объекта obj"
;;   `(get-hash ,obj ,key))

;; (defun get-method (class-name method-name)
;;   "Рекурсивно возвращает тело метода method-name из класса class-name"
;;   (if (null class-name) (error (concat "no method " (symbol-name method-name)))
;;     (let ((class (slot *class-table* class-name)))
;;       (if (check-key class method-name)
;; 	  (slot class method-name)
;; 	(get-method (slot class 'parent) method-name)))))

;; (defmacro defmethod (name args &rest body)
;;   "Определяет метод с именем name"
;;   "args - аргументы, первый аргумент состоит из имени экземпляра объекта и имени класса"
;;   "body - тело метода"
;;   `(let ((class (slot *class-table* ',(cadar args))))
;;      (set-hash class ',name #'(lambda ,(cons (caar args) (cdr args)) ,@body)))
;;   `(defun ,name ,(cons (caar args) (cdr args))
;;      (funcall (get-method (slot ,(caar args) 'class) ',name) ,(caar args) ,@(cdr args))))

(defmacro super (method-name obj &rest args)
  "Вызов метода method-name родителя экземпляра класса obj с аргументами args"
  `(let ((origclass (slot ,obj 'class)) (res nil))
     (setf (slot ,obj 'class) (slot (slot *class-table* (slot ,obj 'class)) 'parent))
     (setq res (funcall (get-method (slot ,obj 'class) ',method-name) ,obj ,@args))
     (setf (slot ,obj 'class) origclass)
     res))

(defun get-class (obj)
  "Получить символ класса из объекта"
  (get-hash obj 'class))

(defmacro with-slots (class vars obj &rest body)
  "Макрос для связывания полей vars объекта obj с переменными и выполнение действий"
  `(let ,(map #'(lambda(x) `(,x (,(intern (concat (symbol-name class) "-" (symbol-name x))) ,obj))) vars)
     ,@body))
