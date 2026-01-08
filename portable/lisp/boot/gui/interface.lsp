(defvar *screen*)
(defvar *selected*)

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
    `(let ((new-elem (make-instance ,n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
       new-elem)))

(defmacro text (&rest params)
   (let ((n 'text))
     `(let ((new-elem (make-instance ,n)))
	(set-defaults new-elem)
	,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
	new-elem)))

(defmacro block (&rest params)
  (let ((n 'block))
    `(let ((new-elem (make-instance ,n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defmacro vert (&rest params)
  (let ((n 'vert))
    `(let ((new-elem (make-instance ,n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown padding curpos sizeable) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defmacro horiz (&rest params)
  (let ((n 'horiz))
    `(let ((new-elem (make-instance ,n)))
       (set-defaults new-elem)
       ,@(map #'(lambda (elem)
		  (if (contains '(id x y width height back-colour active-colour parent keyup keydown padding curpos sizeable) (car elem))
		      `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))
	      `(add-child new-elem ,elem)))
		    params)
       new-elem)))

(defun update-screen ()
  "Перерисовать экран"
  (gsave)
  (draw *screen*)
  (grestore)
  (draw-screen))

(defmacro set-screen (&rest params)
  "Задать экран"
  `(let ((b (block (width *screen-width*) (height *screen-height*) (back-colour +black+) ,@params)))
     (setq *screen* b)
     (setq *selected* (list (block-children b)))
     (update-screen)))

(defun svap-colours (elem)
  "Поменять активный и фоновый цвета"
  (let* ((bc (element-back-colour elem)))
    (element-set-back-colour elem (element-active-colour elem))
    (element-set-active-colour elem bc))
  (update-screen))

(defun next-selected ()
  "Переключить на следующий выделенный элемент"
  (let* ((prev (caar *selected*))
  	 (children (element-children prev)))
    (svap-colours prev)
    (if (null children)
  	(let ((cur (cdar *selected*)))
  	  (if (null cur)
  	      (progn (setq *selected* (cdr *selected*))
  		     (while (and (not (null (cdr *selected*)))
  				 (null (car *selected*)))
  			    (setq *selected* (cdr *selected*)))
  		     (if (null (cdr *selected*))
  			 (setq *selected* (list (block-children *screen*)))
  			 (setq *selected* (cdr *selected*))))
  	      (rplaca *selected* cur)))
  	(progn (rplaca *selected* (cdar *selected*))
  	       (setq *selected* (append (list children) *selected*))))
    (svap-colours (caar *selected*))
    (update-screen)))

(setq *key-down-handler*
      #'(lambda (key)
	  (if (= key +key-tab+) (next-selected)
	      nil
	      ;;(funcall (elem-keydown ?selected?) key))
)))

(setq *key-up-handler*
      #'(lambda (key)
	  nil
	  ;;(funcall (elem-keyup ?selected?) key))
	  ))
