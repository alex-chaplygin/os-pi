;; Максимальное количество классов
(defconst +max-class-count+ 100)

;; Максимальное количество методов
(defconst +max-methods-count+ 100)

;; Индекс последнего созданного класса
(defvar *last-class* 0)
;; Индекс последнего созданного метода
(defvar *last-method* 0)

;; Индекс слотов в массиве класса
(defconst +slots-id+ 0)
;; Индекс родителя в массиве класса
(defconst +parent-id+ 1)
;; Индекс количества полей класса
(defconst +slots-count-id+ 2)
;; Поле номера класса в объекте
(defconst +class-id+ 0)
;; Индекс первого поля класса в объекте
(defconst +slot-id+ 1)
 
;; Массив классов
;; Класс - это массив массивов, len = 4
;; [0] - список полей класса
;; [1] - номер родителя
;; [2] - количество полей класса
(defvar *class-table* (make-array +max-class-count+))

;;Массив имен классов (имена хранятся в форме символов)
(defvar *class-names* (make-array +max-class-count+))

;;Массив имен методов (имена методов хранятся в форме символов)
(defvar *methods-names* (make-array +max-methods-count+))

;;Хеш-таблица методов (хеш по class-id + method-id)
(defvar *methods* (make-hash))

(defun make-indexing-list (list start)
  "Преобразует список в список точечных пар (индекс . значение)"
  (if (null list) nil
      (append (list (cons start (car list))) (make-indexing-list (cdr list) (++ start)))))

(defun get-class-id (class-name)
  "Получение id класса"
  (if (null class-name) nil
    (labels ((search (index)
	       (if (= index *last-class*) nil
		 (if (eq class-name (aref *class-names* index)) index
		   (search (++ index))))))
	    (search 0))))

(defun check-class-type (child-class-id target-class-id)
  "Проверяет принадлежит ли класс или его родители к target-class"
  (if (null child-class-id) nil
      (if (= child-class-id target-class-id) T
	  (let ((child-class (aref *class-table* child-class-id)))
            (if (null child-class) nil
		(check-class-type (aref child-class +parent-id+) target-class-id))))))

(defmacro gen-class-functions (class indexed-slots)
  "Макрос для генерация функций селекторов - <class>-<slot>,
   функций мутаторов - <class>-set-<slot>,
   функции предиката принадлежности класса"
  `(progn
     ,@(map #'(lambda (s) `(progn
	      (defun ,(intern (concat (symbol-name class) "-" (symbol-name (cdr s)))) (obj)
              "Генерация селекторов"
                 (aref obj ,(car s)))
              (defun ,(intern (concat (symbol-name class) "-SET-" (symbol-name (cdr s)))) (obj val)
              "Генерация мутаторов"
                 (seta obj ,(car s) val))))
            ',indexed-slots)
     (defun ,(intern (concat (symbol-name class) "P")) (obj)
       "Проверяет, является ли объект экземпляром класса или его потомком"
       (if (null obj) nil
           (check-class-type (aref obj +class-id+) (get-class-id ',class))))))

(defun get-slots-count(class-id)
  "Получение количества полей класса"
  (if (null class-id) 0
    (let ((class (aref *class-table* class-id)))
	(+ (list-length (aref class +slots-id+)) (get-slots-count (aref class +parent-id+))))))

(defun get-slots (class-id)
  "Получение списка свойств класса class"
  (if (null class-id) nil
    (let ((class (aref *class-table* class-id)))
      (if (null class) nil
	(append (get-slots (aref class +parent-id+)) (aref class +slots-id+))))))

(defun make-instance (class-name)
  "Создать экземпляр объекта класса class"
  "(make-instance 'point) -> ((X.nil)(Y.nil))"
  (let* ((index (get-class-id class-name))
	 (class (aref *class-table* index))
	 (obj (make-array (++ (aref class +slots-count-id+))))
	 )
    (seta obj +class-id+ index)
    obj))

(defmacro defclass (name parent slots)
  "Создание нового класса
  name - имя, parent - родительский класс,
  slots - список полей"
  "(defclass point (x y) ())"
  ;; make-point (x y)
  ;; (seta obj 1 x)
  ;; (seta obj 2 y)
  `(let* ((parent-id (get-class-id ',parent))
	  (class (make-array 3)))
     (seta class +slots-id+ ',slots)
     (seta class +parent-id+ parent-id)
     (seta class +slots-count-id+ (+ (get-slots-count parent-id) (list-length ',slots)))
     (seta *class-table* *last-class* class)
     (seta *class-names* *last-class* ',name))
  `(defun ,(intern (concat "MAKE-" (symbol-name name))) ,(get-slots *last-class*)
     "Создает функцию-конструктор для класса"
     (let ((obj (make-instance ',name)))
       ,@(map #'(lambda(s) `(seta obj ,(car s) ,(cdr s)))
	      (make-indexing-list (get-slots *last-class*) +slot-id+))
       obj))
  `(gen-class-functions ,name ,(make-indexing-list (get-slots *last-class*) +slot-id+))
  `(incf *last-class*)
  `',name)

(defun get-method-id (method-name)
  "Получение id метода"
  (if (null method-name) nil
      (labels ((search (index)
		 (if (= index *last-method*) nil
                     (if (eq method-name (aref *methods-names* index)) index
			 (search (++ index))))))
	(search 0))))

(defun dispatch-method (class-id method-id)
  "Ищет реализацию метода method-id класса class-id по родителям"
  (if (null class-id) nil
      (let* ((key (cons method-id class-id))
             (dispatched-method (get-hash *methods* key)))
	(if (null dispatched-method)
            (let ((class (aref *class-table* class-id)))
              (if (null class) nil
		  (dispatch-method (aref class +parent-id+) method-id)))
            dispatched-method))))

(defun get-method (class-id method-id)
  "Рекуррентно возвращает тело метода по id метода из класса class-id"
  (let ((method (dispatch-method class-id method-id)))
    (if (null method) (error "no method " method-id)
        method)))

(defmacro defmethod (name args &rest body)
  "Определяет метод с именем name"
  (let* ((self-arg (caar args))
         (class-sym (cadar args))
         (method-args (cdr args))
         (method-id (get-method-id name)))
    (if (null method-id)
        (progn
          (seta *methods-names* *last-method* name)
          (setq method-id *last-method*)
          (incf *last-method*))
        nil)
    `(let ((class-id (get-class-id ',class-sym)))
       (set-hash *methods* (cons ,method-id class-id)
                 #'(lambda ,(cons self-arg method-args) ,@body))
       (defun ,name (,self-arg ,@method-args)
         (let* ((obj-class-id (aref ,self-arg +class-id+))
                (method-func (get-method obj-class-id ,method-id)))
           (funcall method-func ,self-arg ,@method-args))))))

(defmacro super (method-name obj &rest args)
  "Вызов метода method-name родителя экземпляра класса obj с аргументами args"
  `(let* ((class-id (aref ,obj +class-id+))
          (parent-class-id (aref (aref *class-table* class-id) +parent-id+))
          (method-id (get-method-id ',method-name)))
     (if (null parent-class-id) 
         (error (concat "no parent for class " class-id))
         (funcall (get-method parent-class-id method-id) ,obj ,@args))))

(defmacro with-slots (class vars obj &rest body)
  "Макрос для связывания полей vars объекта obj с переменными и выполнение действий"
  `(let ,(map #'(lambda(x) `(,x (,(intern (concat (symbol-name class) "-" (symbol-name x))) ,obj))) vars)
     ,@body))
