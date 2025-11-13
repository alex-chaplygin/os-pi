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
  "Элементарный парсер, ожидающий заданный элемент из потока"
  (parse-pred #'(lambda (x) (= x sym))))

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
		     
(defun parse-app (parser f)
  "Комбинатор применения функции к результату разбора"
  #'(lambda (stream)
      (let ((r (funcall parser stream)))
	(if (null r) nil (cons (funcall f (car r)) (cdr r))))))

(defun parse-return (res)
  #'(lambda (x) res))

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений заданного парсера. Возвращает список результатов"
  #'(lambda (stream)
      (labels ((apply (stream res)
		      (let ((parser-res (funcall parser stream)))
			(if (null parser-res) (cons res stream)
			  (apply (cdr parser-res) (append res (list (car parser-res))))))))
	      (apply stream nil))))

(defun parse-optional (parser)
  "Комбинатор: 0 или 1 применение парсера.
   Всегда успешен. Возвращает (значение . поток), где значение = nil, если парсер не сработал."
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if res res
            (cons nil stream)))))

(defun parse-some (parser)
  "Комбинатор - 1 или более повторений заданного парсера. Возвращает список результатов"
  (parse-app (&&& parser (parse-many parser))
	     #'(lambda (x) (cons (car x) (second x)))))

(defun parse-sep (parser sep)
  "Комбинатор - список парсеров, разделенных парсером sep"
  (parse-many (parse-app (&&& (parse-optional sep) parser) #'second)))

(defmacro parse-rec (parser)
  "Комбинатор для рекурсивных парсеров"
  `#'(lambda (stream) (funcall ,parser stream)))

(defun parse-hex ()
  "Разбор шестнадцатеричного числа вида 0xFF"
  (parse-app
    (&&& (parse-elem #\0)
         (parse-elem #\x)
         (parse-pred #'is-hex-sym)
         (parse-many (parse-pred #'is-hex-sym)))
    #'(lambda (parts)
        (strtoint (implode (cons (third parts) (forth parts))) 16))))

(defun parse-decimal ()
  "Разбор десятичного числа (поддерживает -123)"
  (parse-app
    (&&& (parse-optional (parse-elem #\-))   ; ← необязательный минус
         (parse-pred #'is-digit)
         (parse-many (parse-pred #'is-digit)))
    #'(lambda (parts)
	(let ((num (strtoint (implode (cons (second parts) (third parts))) 10)))
            (if (car parts) (- num) num)))))

(defun parse-array (arr)
  "Ожидание в потоке заданного массива"
  #'(lambda (stream)
      (let ((res (get-array stream (array-size arr))))
	(if (null res) nil
	  (if (= arr (car res)) res nil)))))

(defun parse-struct (struct)
  "Читаем структуру из потока"
  #'(lambda (stream)
      (get-struct stream struct)))
