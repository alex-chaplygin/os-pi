(defvar *gui-screen*)
(defvar *gui-selected*)

(defmacro gen/elem (name)
  "Генерация макроса для класса name"
  "Макрос создает объект с заданным списком свойств"
  `(defmacro ,name (&rest params)
     ;;(let ((n ',name))
       `(let ((new-elem (make-instance ',name)))
	  (set-defaults new-elem)
	  ,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name ',name) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
	  new-elem)))

;;(gen/elem element)
;(gen/elem text)

(defmacro element (&rest params)
  (let ((n 'element))
    `(let ((new-elem (make-instance ',n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
       new-elem)))

(defmacro text (&rest params)
   (let ((n 'text))
     `(let ((new-elem (make-instance ',n)))
	(set-defaults new-elem)
	,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
	(unless (text-width new-elem)
	  (text-set-width new-elem (string-size (text-text new-elem)))
	  (text-set-height new-elem 1))
	new-elem)))

(defmacro block (&rest params)
  (let ((n 'block))
    `(let ((new-elem (make-instance ',n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defmacro vert (&rest params)
  (let ((n 'vert))
    `(let ((new-elem (make-instance ',n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown padding curpos sizeable) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defmacro horiz (&rest params)
  (let ((n 'horiz))
    `(let ((new-elem (make-instance ',n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown padding curpos sizeable) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defun update-screen ()
  "Перерисовать экран"
  (draw *gui-screen*)
  (draw-screen))

(defun swap-colours (elem)
  "Поменять активный и фоновый цвета"
  (let* ((bc (element-back-colour elem)))
    (element-set-back-colour elem (element-active-colour elem))
    (element-set-active-colour elem bc)))

(defmacro set-screen (&rest params)
  "Задать экран"
  `(let ((b (block (width *screen-width*) (height *screen-height*) (back-colour +black+) ,@params)))
     (setq *gui-screen* b)
     (setq *gui-selected* (list b))
     (swap-colours b)
     (update-screen)))

(defun next-selected ()
  "Переключить на следующий выделенный элемент"
  (let* ((prev (car *gui-selected*))
  	 (children (element-children prev)))
    (swap-colours prev)
    (if (null children)
    	;; переходим на следующий элемент в списке если есть
    	(setq *gui-selected* (cdr *gui-selected*))
    	;; добавляем дочерние элементы в начало списка
    	(setq *gui-selected* (append children (cdr *gui-selected*))))
    (when (null *gui-selected*)
      (setq *gui-selected* (list *gui-screen*)))
    (swap-colours (car *gui-selected*))
    (update-screen)))

(defun get-element-by-id (id)
  "Найти элемент с id, возвращает или элемент или nil"
  (labels ((search-id (node) ;; возвращает t или nil если id соответствует
	     (if (= id (element-id node)) t nil))
	   (search-list (list)
	     (if (null list) nil ;; список закончился - не найдено
		 (let ((el (search-tree (car list)))) ;; проверяем первый элемент
		   (if el el (search-list (cdr list)))))) ;; проверяем остальные
	   (search-tree (node) ;; поиск для группового элемента
	     (if (search-id node) node ;; проверяем сам элемент
		 (search-list (element-children node))))) ;; проверяем все дочерние элементы
    (search-tree *gui-screen*)))

(setq *key-down-handler*
      #'(lambda (key)
	  (if (= key +key-tab+) (next-selected) 
	      (let ((handler (element-keydown (car *gui-selected*))))
	      	(when handler (funcall handler key))))))

(setq *key-up-handler*
      #'(lambda (key)
	  (let ((handler (element-keyup (car *gui-selected*))))
	    (when handler (funcall handler key)))))
