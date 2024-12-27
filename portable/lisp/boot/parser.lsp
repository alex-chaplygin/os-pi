(defun parse-elem (sym)
  "функция, которая создает элементарный парсер, ожидающий заданный элемент в списке"
  #'(lambda (list)
      (list (if (= sym (car list))
		(cons sym (cdr list))
		(cons nil list)))))

(defun &&& (transform &rest parsers)
  "Создает последовательный парсер, который применяет несколько парсеров parsers подряд к списку.
   Если все парсеры успешно отработали, то результат преобразуется через указанную функцию transform"
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null parsers)
		       (cons (funcall transform res) list)
		     (let ((parser-res (car (funcall (car parsers) list))))
		       (unless (null (car parser-res))
			 (apply-parser (cdr parsers) (cdr parser-res) (append res (list (car parser-res)))))
		       ))))
	(let ((res (apply-parser parsers list nil)))
	  (if (null res) (list (cons nil list)) (list res))))))

(defun parse-or (&rest parsers)
  "Принимает список парсеров parsers, возвращая список результатов успешных парсеров.
   Правила применяются только к первому элементу списка."
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (list)
      (let ((parsers-list parsers)
	    (result nil))
	(while parsers-list
	  (let ((parser-result (funcall (car parsers-list) list)))
	     (setq parsers-list (cdr parsers-list))
	    (when (caar parser-result)
	      (setq result parser-result)
	      (setq parsers-list nil))))
	(if result
	    result
	    (list (cons nil list)))
	)))

(defun parse-many (parser)
  "Принимает парсер, применяя его к списку, уменьшая список и
   сохраняя по одному результату парсера на каждом этапе, если он успешный"
  #'(lambda (list)
      (let ((result nil)
	    (rest nil))
	(while list
	  (let ((parser-result
		 (car (funcall parser list))))
	    (if (car parser-result)
		(progn
		  (setq result (append result (list (car parser-result))))
		  (setq list (cdr parser-result)))
		  (progn
		    (setq rest (cdr parser-result))
		    (setq list nil)))))
	(list (cons result rest)))))

(defun parse-pred (pred)
  "Парсинг по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t.
   Сама функция парсинга возвращает в результате парсинга этот символ"
  #'(lambda (list)
      (list (if (funcall pred (car list))
		(cons (car list) (cdr list))
		(cons nil list)))))

(defun parse-atom ()
  "Создает парсер, который распознает атомы"
  #'(lambda (str)
      (let ((res (funcall (parse-many (parse-or (parse-pred #'is-alpha) (parse-pred #'is-digit))) str)))
	(list (cons (intern (implode (caar res))) (cdar res))))))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп"
  (caar (funcall (parse-atom) (explode str))))
