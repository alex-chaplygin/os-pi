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
  "Пропуск 0 или более пробелов"
  #'(lambda (str)
      (let ((res (funcall (parse-many (parse-elem #\ )) str)))
	(list (cons #\ (cdar res))))))

(defun parse-atom ()
  "Разбор атома, начинается с буквы, содержит хотя бы одну букву."
  #'(lambda (str)
      (let ((res (funcall (parse-or
			               (parse-tnumber)
			               (parse-tsymbol)
                           (parse-tchar)
                           (parse-tfunction)
                           (parse-tstring))
			              str)))
        (if res (list (car res))
	        nil))))

(defun parse-char (char)
  "Разбор символа char с учетом пробелов"
  (&&& #'cadr (skip-spaces) (parse-elem char)))

(defun strtoint (str base)
  "Конвертирует строку str в число в системе счисления base"
  (let ((res 0))
    (for i 0 (string-size str)
         (let* ((char (toupper (char str i)))
                (c (char-code char))
                (c-num 0))
           (if (and (>= c (char-code #\A))
                    (<= c (char-code #\Z)))
               (setq c-num (+ (- c (char-code #\A)) 10)
                     res (+ (* res base)
                            (if (< c-num base)
                                c-num
                                (error "strtoint: invalid digit"))))
               (setq c-num (- c (char-code #\0))
                     res (+ (* res base) c-num)))))
    res))

(defun toupper (char)
  "Преобразовать символ char в верхний регистр"
  (let ((c (char-code char)))
    (if (and (>= c (char-code #\a))
             (<= c (char-code #\z)))
        (code-char (- c (- (char-code #\a) (char-code #\A))))
        char)))

(defun parse-decimal ()
  "Разбор десятичного числа"
  (&&& #'(lambda (l) (strtoint (implode (cons (cadr l) (caddr l))) 10))
       (skip-spaces)
	   (parse-pred #'is-digit)
	   (parse-many (parse-pred #'is-digit))))

(defun is-hex-sym (sym)
  "Предикат проверки на символ шестнадцатеричного числа"
  (let ((c (char-code sym)))
    (or (is-digit sym) (and (>= c 65) (<= c 70)) (and (>= c 97) (<= c 102)))))

(defun parse-hex ()
  "Разбор шестнадцатеричного числа"
  (&&& #'(lambda (l) (strtoint (implode (cons (cadddr l) (caddddr l))) 16))
       (skip-spaces)
	   (parse-char #\0)
	   (parse-char #\x)
       (parse-pred #'is-hex-sym)
	   (parse-many (parse-pred #'is-hex-sym))))

(defun parse-tnumber ()
  "Разбор десятичного или шестнадцатеричного числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun is-symbol (sym)
  "Предикат проверки на особый символ"
  (case sym
    (#\+ t) (#\- t) (#\* t) (#\/ t) (#\= t) (#\_ t) (#\& t) (#\| t) (#\< t) (#\> t) (#\% t)
    (otherwise nil)))

(defun parse-tsymbol ()
  "Разбор символа"
  (&&& #'(lambda (l) (intern (implode (map #'(lambda (char) (toupper char)) (cons (cadr l) (caddr l))))))
       (skip-spaces)
       (parse-or (parse-pred #'is-alpha)
                 (parse-pred #'is-symbol))
       (parse-many
        (parse-or (parse-pred #'is-alpha)
                  (parse-pred #'is-digit)
                  (parse-pred #'is-symbol)))))

(defun parse-tchar ()
  "Разбор ASCII-символа"
  (&&& #'(lambda (l) (cadddr l))
       (skip-spaces)
       (parse-char #\#)
       (parse-char #\\)
       (parse-pred #'(lambda (sym) t))))

(defun parse-tfunction ()
  "Разбор символа функции"
  (&&& #'(lambda (l) (list 'FUNCTION (cadddr l)))
       (skip-spaces)
       (parse-char #\#)
       (parse-char #\')
       (parse-s)))

(defun parse-escape ()
  "Разбор экранирования"
  (&&& #'(lambda (l) (list (char "\n" 0)))
       (parse-char #\\)
       (parse-char #\n)))

(defun parse-tstring ()
  "Разбор строки"
  (&&& #'(lambda (l) (implode (caddr l)))
       (skip-spaces)
       (parse-char #\")
       (parse-many (parse-or (parse-escape)
                             (parse-pred #'(lambda (sym) (!= sym #\")))))
       (parse-char #\")))

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
