
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
        (if (null r)
            nil
            (cons (funcall f (car r)) (cdr r))))))

(defun parse-atom ()
  "Парсер атома: число, символ, строка, char, функция"
  (parse-or (parse-tnumber)
            (parse-tstring)
            (parse-tchar)
            (parse-tfunction)
            (parse-tsymbol)))

(defun parse-list ()
  (parse-app
    (&&& (parse-elem #\()
         (parse-many (parse-app (&&& (skip-spaces) (lambda (s) (funcall *parse-top-level* s)))
                                #'cadr))
         (skip-spaces)
         (parse-elem #\)))
    #'(lambda (parts)
        (cadr parts)))) ; второй элемент — список


(defun parse-many (parser)
  "Комбинатор - 0 или более повторений заданного парсера. Возвращает список результатов"
  #'(lambda (stream)
      (labels ((apply (stream res)
                 (let ((parser-res (funcall parser stream)))
                   (if (null parser-res)
                       (cons res stream)  ; ← всегда возвращаем (cons res stream), даже если res = nil
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
	     #'(lambda (x) (cons (car x) (cadr x)))))

(defun skip-spaces ()
  "Пропуск 0 или более пробелов"
  (parse-many (parse-elem #\ )))


(defun parse-hex ()
  "Разбор шестнадцатеричного числа вида 0xFF"
  (parse-app
    (&&& (parse-elem #\0)
         (parse-elem #\x)
         (parse-pred #'is-hex-sym)
         (parse-many (parse-pred #'is-hex-sym)))
    #'(lambda (parts)
        (let ((first-digit (caddr parts))        ; 3-й элемент
              (rest-digits (cadddr parts)))      ; 4-й элемент — список
          (strtoint (implode (cons first-digit rest-digits)) 16)))))

(defun parse-decimal ()
  "Разбор десятичного числа (поддерживает -123)"
  (parse-app
    (&&& (parse-optional (parse-elem #\-))   ; ← необязательный минус
         (parse-pred #'is-digit)
         (parse-many (parse-pred #'is-digit)))
    #'(lambda (parts)
        (let ((minus (car parts))             ; nil или #\-
              (first-digit (cadr parts))
              (rest-digits (caddr parts)))
          (let ((num (strtoint (implode (cons first-digit rest-digits)) 10)))
            (if minus (- num) num))))))

(defun parse-tnumber ()
  "Парсер числа: поддерживает десятичные (123, -456) и шестнадцатеричные (0xFF) литералы"
  #'(lambda (stream)
      (let ((hex-res (funcall (parse-hex) stream)))
        (if hex-res
            hex-res
            (funcall (parse-decimal) stream)))))


(defun parse-tsymbol ()
  (parse-app
    (&&& (parse-pred #'is-symbol-start)
         (parse-many (parse-pred #'is-symbol-char)))
    #'(lambda (parts)
        (let ((first (car parts))
              (rest (cadr parts)))
          (intern (implode (cons first rest)))))))  ; ← без string-downcase

(defun parse-tchar ()
  (parse-app
    (&&& (parse-elem #\#)
         (parse-elem #\\)
         (parse-pred #'is-not-quote))
    #'(lambda (parts)
        (caddr parts))))

(defun parse-tstring ()
  (parse-app
    (&&& (parse-elem #\")
         (parse-many (parse-pred #'is-not-double-quote))
         (parse-elem #\"))
    #'(lambda (parts)
        (implode (cadr parts)))))

(defun parse-tfunction ()
  "Парсер функции: #'f или #'(lambda ...)"
  (parse-app
    (&&& (parse-elem #\#)
         (parse-elem #\')
         (parse-or (parse-list) (parse-atom)))
    #'(lambda (parts)
        (list 'function (caddr parts)))))

(defun parse-tarray ()
  "Парсер массива: #(1 2 3)"
  (parse-app
    (&&& (parse-elem #\#)
         (parse-elem #\()
         (parse-many (parse-app (&&& (skip-spaces) (parse-tnumber))
                                #'cadr))
         (skip-spaces)
         (parse-elem #\)))
    #'(lambda (parts)
        (apply vector (caddr parts))))) ; третий элемент — список чисел
