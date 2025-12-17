
;; (defun screen (&rest windows)
;;   "Отрисовка окон"
;;   "windows - список окон"
;;   (app #'(lambda (w) (draw w)) windows))

(defmacro gen/elem (name)
  "Генерация макроса для класса name"
  "Макрос создает объект с заданным списком свойств"
  `(defmacro ,name (&rest params)
     (let ((n ',name))
       `(let ((new-elem (make-instance ,n)))
	  (set-defaults new-elem)
	  ,@(map #'(lambda (elem) `( ,(intern (concat (symbol-name n) "-SET-" (symbol-name (car elem)))) new-elem ,(second elem))) params)
	  new-elem))))

;(gen/elem element)
;(gen/elem text)
;(gen/elem edit)

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

;(setq elem (make-edit 10 10 0 0 12 6 "test 12 test test 111111111 222222222 33333333 444444444" +cyan+ +cyan+ +green+ nil nil 0 '(0 . 0) #(0 0 0 0)))

;; (setq w (window "test1" (width 20) (height 10)
;; 	 (children
;; 	  (text "abc" (text "123") (color +green+))
;; 	  (text "1111155555557777" (width 5) (height 5))
;; 	  (text "abcd" (x 10) (y 10))
;; 	  (edit "abcddd" (x 10) (y 15)))))
;; (screen
;;  w
;;  (window "test111111111111111111111111111111111111111111")
;;  (window "test3" (height 15) (back-color +green+))
;;  (window "test4" (width 20) (height 10)))
