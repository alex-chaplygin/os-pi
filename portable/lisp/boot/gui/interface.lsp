(defvar *window-list* nil) ; Список окон
(defvar *current-window-pos* '(0 . 0)) ; Нынешняя позиция окна
(defvar *max-windows-bottom* 0) ; Нижняя граница окна

(defmacro window (text &rest params)
  "Создание нового окна"
  "text - заголовок окна"
  "params - свойства окна"
  `(let* ((new-window (make-instance window))
	  (x (car *current-window-pos*))
          (y (cdr *current-window-pos*))
	  (width 0)
	  (height 0))
     (set-defaults new-window)
     (setf (slot new-window 'text) ,text)
     (setq *window-list* (append (list new-window) *window-list*))
     (setf (slot new-window 'x) x)
     (setf (slot new-window 'y) y)     
     (setf (slot new-window 'width) (+ (slot new-window 'width) (string-size ,text)))
     ,@(map '(lambda (elem) `(if (equal ',(car elem) 'children)
				 (window-add-children new-window ',(cdr elem))
			       (setf (slot new-window ',(car elem)) ,(cadr elem))))
	    params)
     (setq width (slot new-window 'width))
     (setq height (slot new-window 'height))
     (when (> (+ y height) *max-windows-bottom*) (setq *max-windows-bottom* (+ y height)))
     (when (> (+ x width) *screen-width*)
       (setq x 0)
       (setq y *max-windows-bottom*)
       (setq *max-windows-bottom* (+ y height))
       (setf (slot new-window 'x) x)
       (setf (slot new-window 'y) y))       
     (setq *current-window-pos* (cons (+ x width) y))
     new-window))

(defun window-add-children (window children)
  "Добавить список дочерних элементов к окну, вычислить каждый элемент перед добавлением"
  (unless (null children)
    (add-child window (eval (car children)))
    (window-add-children window (cdr children))))

(defun screen (&rest windows)
  "Отрисовка окон"
  "windows - список окон"
  (app '(lambda (w) (draw w)) windows))

(defmacro gen/elem (name)
  "Генерация макроса для класса name"
  "Макрос создает объект с заданным списком свойств"
  `(defmacro ,name (text &rest params)
     (let ((n ',name))
       `(let ((new-elem (make-instance ,n)))
	  (setf (slot new-elem 'text) ,text)
	  (set-defaults new-elem)
	  ,@(map '(lambda (elem) `(setf (slot new-elem ',(car elem)) ,(cadr elem))) params)
	  new-elem))))

(gen/elem text)
(gen/elem edit)

;(setq elem (make-edit 10 10 0 0 12 6 "test 12 test test 111111111 222222222 33333333 444444444" +cyan+ +cyan+ +green+ nil nil 0 '(0 . 0) #(0 0 0 0)))

(setq w (window "test1" (width 20) (height 10)
	 (children
	  (text "abc" (text "123") (color +green+))
	  (text "1111155555557777" (width 5) (height 5))
	  (text "abcd" (x 10) (y 10))
	  (edit "abcddd" (x 10) (y 15)))))
(screen
 w
 (window "test111111111111111111111111111111111111111111")
 (window "test3" (height 15) (back-color +green+))
 (window "test4" (width 20) (height 10)))
