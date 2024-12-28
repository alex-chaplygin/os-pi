(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент в списке"
  #'(lambda (list)
      (if (null list) nil        
	  (if (= sym (car list))
	      (list (cons sym (cdr list)))
	      nil))))

(defun &&& (transform &rest parsers)
  "Последовательный комбинатор применяет несколько парсеров подряд к списку, каждый следующий parser применяется к остатку от работы предыдущего parser.
Если все парсеры успешно отработали, то результат как список передается в указанную функцию transform"
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null list) (list (list (funcall transform res)))
		     (if (null parsers)
			 (if (null res) nil
			     (list (cons (funcall transform res) list)))
			 (let ((parser-res (funcall (car parsers) list)))
			   (if (null parser-res) nil
			       (apply-parser (cdr parsers) (cdar parser-res) (append res (list (caar parser-res))))))))))
	(apply-parser parsers list nil))))

(defun parse-or (&rest parsers)
  "Параллельный комбинатор принимает список парсеров parsers, объединяя результаты разбора всех парсеров."
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null parsers) res
		     (let ((parser-res (funcall (car parsers) list)))
		       (apply-parser (cdr parsers) list (append res parser-res))))))
      (apply-parser parsers list nil))))
		     

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений заданного парсера. Возвращает список результатов"
 #'(lambda (list)
     (labels ((apply (list res)
		(if (null list) (list (list res))
		    (let ((parser-res (funcall parser list)))
		      (if (null parser-res)
			  (list (cons res list))
			  (apply (cdar parser-res) (append res (list (caar parser-res)))))))))
       (apply list nil))))

(defun parse-pred (pred)
  "Парсер по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t.
   Сама функция парсинга возвращает в результате парсинга в случае успешного разбора сам символ, в случае неудачного - nil."
  #'(lambda (list)
      (if (null list) nil
	  (if (funcall pred (car list))
	      (list list)
	      nil))))

(defun skip-spaces ()
  "Пропуск 1 или более пробелов"
  #'(lambda (str)
      (let ((res (funcall (parse-many (parse-elem #\ )) str)))
	(list (cons #\ (cdar res))))))

(defun parse-atom ()
  "Разбор атома, начинается с буквы, содержит хотя бы одну букву."
  #'(lambda (str)
      (let ((res (funcall (&&& #'(lambda (l) (cons (cadr l) (caddr l)))
			       (skip-spaces)
			       (parse-pred #'is-alpha)
			       (parse-many (parse-pred #'is-alpha)))
			  str)))
	(if res (list (cons (intern (implode (caar res))) (cdar res)))
	    nil))))

(defun parse-char (char)
  "Разбор символа char с учетом пробелов"
  (&&& #'cadr (skip-spaces) (parse-elem char)))

(defun parse-list ()
  "Разбор списка s-выражений"
  #'(lambda (str)
      (let ((res (funcall (&&& #'cadr
			       (parse-char #\()
			       (parse-many (parse-s))
			       (parse-char #\)))
			  str)))
	res)))

(defun parse-s ()
  "Разбор s-выражения"
  (parse-or (parse-atom) (parse-list)))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп"
  (caar (funcall (parse-s) (explode str))))
