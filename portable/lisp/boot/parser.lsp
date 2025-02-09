(defun parse-pred (pred)
  "Парсер по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t.
   Сама функция парсинга возвращает в результате парсинга в случае успешного разбора сам символ, в случае неудачного - nil."
  #'(lambda (list)
      (if (null list) nil
	  (if (funcall pred (car list)) (list list) nil))))

(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент в списке"
  (parse-pred #'(lambda (x) (= x sym))))

(defun &&& (transform &rest parsers)
  "Последовательный комбинатор применяет несколько парсеров подряд к списку, каждый следующий parser применяется к остатку от работы предыдущего parser.
Если все парсеры успешно отработали, то результат как список передается в указанную функцию transform"
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null parsers)
		     (if (null res) nil
			 (list (cons (funcall transform res) list)))
		     (let ((parser-res (funcall (car parsers) list)))
		       (if (null parser-res) nil
			   (apply-parser (cdr parsers)
                                         (when (cdar parser-res) (cdar parser-res))
                                         (append res (list (caar parser-res)))))))))
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
		     
(defun parse-app (parser f)
  "Комбинатор применения функции ко всем результатам разбора"
  #'(lambda (list)
      (let ((res (funcall parser list)))
	(map #'(lambda (r) (cons (funcall f (car r)) (cdr r))) res))))

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

(defun skip-spaces ()
  "Пропуск 0 или более пробелов"
  #'(lambda (str)
      (let ((res (funcall (parse-many (parse-elem #\ )) str)))
 (cons #\ (cdar res))))))
