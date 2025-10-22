(defun parse-suc (val)
  "Элементарный парсер - успешный разбор со значением val"
  #'(lambda (stream) (cons val stream)))

(defun parse-fail ()
  "Элементарный парсер - неудачный разбор"
  #'(lambda (stream) nil))

(defun parse-pred (pred)
  "Парсер по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t.
   Сама функция парсинга возвращает в результате парсинга в случае успешного разбора сам символ, в случае неудачного - nil."
  #'(lambda (stream)
      (if (null stream) nil
	(let ((res (get-byte stream)))
	  (if (null res) nil
	    (if (funcall pred (car res)) res nil))))))

(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент в списке"
  (parse-pred #'(lambda (x) (eq x sym))))


(defun &&& (&rest parsers)
  "Последовательный комбинатор применяет несколько парсеров подряд к потоку, каждый следующий parser применяется к остатку от работы предыдущего parser."
  #'(lambda (stream)
      (labels ((apply-parser (parsers stream res)
		 (if (null parsers) (cons res stream)
		     (let ((parser-res (funcall (car parsers) stream)))
		       (if (null parser-res) nil
			   (apply-parser (cdr parsers) (cdr parser-res)
                                         (append res (list (car parser-res)))))))))
	(apply-parser parsers stream nil))))


(defun parse-or (&rest parsers)
  "Параллельный комбинатор принимает список парсеров parsers и работает до первого успешного разбора"
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (stream)
      (labels ((apply-parser (parsers stream)
		 (if (null parsers) nil
		   (let ((parser-res (funcall (car parsers) stream)))
		     (if (null parser-res) (apply-parser (cdr parsers) stream) parser-res)))))
      (apply-parser parsers stream))))
		     
(defun parse-first (&rest parsers)
  "Комбинатор, который возвращает результат первого успешного парсера."
  (unless parsers (error "parse-first: no parsers"))
  #'(lambda (list)
      (if (null parsers)
          nil
          (let ((res (funcall (car parsers) list)))
            (if res
                res
                (funcall (apply #'parse-first (cdr parsers)) list))))))

(defun parse-app (parser f)
  "Комбинатор применения функции к результату разбора"
  #'(lambda (stream)
      (let ((r (funcall parser stream)))
	(cons (funcall f (car r)) (cdr r)))))

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений заданного парсера. Возвращает список результатов"
  #'(lambda (stream)
      (labels ((apply (stream res)
		      (let ((parser-res (funcall parser stream)))
			(if (null parser-res)
			    (if (null res) nil  (cons res stream))
			  (apply (cdr parser-res) (append res (list (car parser-res))))))))
	      (apply stream nil))))

(defun parse-some (parser)
  "Комбинатор - 1 или более повторений заданного парсера. Возвращает список результатов"
  (parse-app (&&& parser (parse-many parser))
	     #'(lambda (x) (cons (car x) (second x)))))

(defun skip-spaces ()
  "Пропуск 0 или более пробелов"
  (parse-many (parse-elem #\ )))


(defun parse-many-sep (parser sep)
  "Комбинатор - 0 или более повторений с разделителем"
  (parse-first (parse-some-sep parser sep) (parse-suc nil)))

(defun parse-some-sep (parser sep)
  "Комбинатор - 1 или более повторений с разделителем"
  (parse-app (&&& parser (parse-many (parse-app (&&& sep parser) #'cadr)))
	     #'(lambda (x) (cons (car x) (cadr x)))))
