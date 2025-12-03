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

(defmacro parse-and-internal (streams parsers)
  "Накапливает список символов потоков в streams, последовательно раскрывает парсеры"
  (cond ((null parsers)
	 `(cons (list ,@(map #'(lambda (x) `(car ,x)) (cdr streams)))
		(cdr ,(last streams))))
	((eq (car parsers) 'return)
	 `(cons ,(second parsers) (cdr ,(last streams))))
	(t
  (let ((cur (last streams))
	(next (gensym))
	(p (car parsers)))
    (if (and (symbolp p) (let* ((str (symbol-name p))
				(size (string-size str))
				(arrow (subseq str (- size 2) size)))
			   (= arrow "->")))
    `(let ((,next (funcall ,(second parsers) ,(if (eq cur 'stream) 'stream `(cdr ,cur)))))
       (if (null ,next) nil
	   (let ((,(let ((s (symbol-name p)))
		     (intern (subseq s 0 (- (string-size s) 2))))
		   (car ,next)))
	    (parse-and-internal ,(append streams (list next)) ,(cddr parsers)))))
    `(let ((,next (funcall ,p ,(if (eq cur 'stream) 'stream `(cdr ,cur)))))
       (if (null ,next) nil
	 (parse-and-internal ,(append streams (list next)) ,(cdr parsers)))))))))

(defmacro &&& (&rest parsers)
  "Последовательный комбинатор применяет несколько парсеров подряд к потоку, каждый следующий parser применяется к остатку от работы предыдущего parser."
  `#'(lambda (stream)
       (parse-and-internal (stream) ,parsers)))

(defun parse_and (&rest parsers)
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

(defun parse-many-n (n parser)
  "Комбинатор - n повторений заданного парсера. Возвращает список результатов"
  #'(lambda (stream)
      (labels ((apply (stream n res)
		 (if (= n 0) (cons res stream)
		     (let ((parser-res (funcall parser stream)))
		       (if (null parser-res) nil
			   (apply (cdr parser-res) (-- n) (append res (list (car parser-res)))))))))
	      (apply stream n nil))))

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

(defmacro mk/elem-parse(name func &rest param)
  "Создать функцию-парсер ожидающую значение p"
  `(defun ,name (p)
    #'(lambda (stream)
	(let ((res (,func stream ,@param)))
	  (if (null res) nil
	      (if (= p (car res)) res nil))))))

(defmacro mk/elem-parse1(name func)
  "Создать функцию-парсер с параметром n ожидающую значение p"
  `(defun ,name (n p)
    #'(lambda (stream)
	(let ((res (,func stream n)))
	  (if (null res) nil
	      (if (= p (car res)) res nil))))))

;; Ожидание в потоке заданного массива
(mk/elem-parse parse-elem-array get-array (array-size p))
;; Ожидание в потоке заданного слова
(mk/elem-parse parse-elem-word get-word)
;; Ожидание в потоке заданных бит
(mk/elem-parse1 parse-elem-bits get-bits)

(defmacro mk/parse1 (name func)
  "Создать функцию разбора на основе функции которая имеет один параметр, кроме потока"
  `(defun ,name (p)
     #'(lambda (stream)
	 (,func stream p))))

(mk/parse1 parse-struct get-struct)
(mk/parse1 parse-bits get-bits)
(mk/parse1 parse-array get-array)
