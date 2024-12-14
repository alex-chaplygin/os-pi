(defvar *actor-classes* (make-hash)) ;; классы акторов
(defvar *actor-prop-vals* (make-hash)) ;; значения по умолчанию для классов

(defmacro actor-class (name parent &rest props)
  "Создание класса актора с названием name, родительским классом parent и стандартными свойствами props"
  `(defclass ,name ,parent ,(get-vars props))
  `(set-hash *actor-prop-vals* ',name
	     (if (null ',parent) ',props
		 (append (get-hash *actor-prop-vals* ',parent) ',props)))
  `',name)

(defmacro make-actor (actor-type &rest props)
   "Cоздание нового актора с именем name из варианта актора actor-type"
   "и опционально изменёнными свойствами props"
  `(let ((new-actor (make-instance ,actor-type)))
     (app #'(lambda (prop) (actor-set-prop new-actor (car prop) (cadr prop)))
	  (append (get-hash *actor-prop-vals* ',actor-type) ',props))
     new-actor))

(defun actor-set-prop (actor prop val)
  "Установить значение свойства prop на val у актора actor"
  (setf (slot actor prop) val))

(defun actor-get-prop (actor prop)
  "Получить значение свойства prop у актора actor"
  (slot actor prop))
