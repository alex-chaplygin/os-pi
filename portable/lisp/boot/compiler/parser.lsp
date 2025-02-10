(defun parse-char (char)
  "Разбор символа char с учетом пробелов"
  (parse-app (&&& (skip-spaces) (parse-elem char)) #'second))

(defun parse-decimal ()
  "Разбор десятичного числа"
  (parse-app (&&& (skip-spaces) (parse-some (parse-pred #'is-digit)))
	     #'(lambda (l) (strtoint (implode (second l)) 10))))

(defun parse-hex ()
  "Разбор шестнадцатеричного числа"
  (parse-app (&&& (skip-spaces) (parse-char #\0) (parse-elem #\x)
		  (parse-some (parse-pred #'is-hex-sym)))
   #'(lambda (l) (strtoint (implode (forth l)) 16))))

(defun parse-tnumber ()
  "Разбор десятичного или шестнадцатеричного числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\%) sym))

(defun parse-tsymbol ()
  "Разбор символа"
  (parse-app (&&& (skip-spaces) (parse-or (parse-pred #'is-alpha)
					  (parse-pred #'is-lisp-symbol))
		  (parse-many (parse-or (parse-pred #'is-alpha)
					(parse-pred #'is-digit)
					(parse-pred #'is-lisp-symbol))))
	     #'(lambda (l) (intern (implode (map #'(lambda (char) (toupper char)) (cons (second l) (third l))))))))

(defun parse-tchar ()
  "Разбор lisp-символа #\\"
  (parse-app (&&& (parse-char #\#) (parse-elem #\\) #'(lambda (l) (list l)))
	     #'third))

(defun parse-tfunction ()
  "Разбор символа функции '#"
  (parse-app (&&& (parse-char #\#) (parse-elem #\') (parse-s))
	     #'(lambda (l) (list 'FUNCTION (third l)))))

(defun parse-escape (char val)
  "Разбор экранирования"
  (parse-app (&&& (parse-char #\\) (parse-elem char)) #'(lambda (l) (list val))))

(defun parse-tstring ()
  "Разбор строки"
  (parse-app
   (&&& (parse-char #\")
	(parse-many (parse-or (parse-escape #\n (code-char 0xa))
			      (parse-pred #'(lambda (sym) (!= sym #\")))))
	(parse-elem #\"))
   (o #'implode #'second)))

(defun parse-tarray ()
  "Разбор массива"
  (parse-app (&&& (parse-char #\#) (parse-list)) (o #'list-to-array #'second)))

(defun parse-atom ()
  "Разбор атомарного объекта."
  #'(lambda (l)
      (let ((r (funcall (parse-or (parse-tnumber) (parse-tsymbol)
  	    ;;(parse-tarray)
  	    (parse-tchar)
  	    ;;(parse-tfunction)
  	    (parse-tstring)) l)))
	(print `(parse-atom ,r)))))

(defun parse-list ()
  "Разбор списка s-выражений"
  (parse-app (&&& (parse-char #\() (parse-many (parse-s)) (parse-char #\)))
	     #'second))
  
(defun parse-s ()
  "Разбор s-выражения"
  #'(lambda (l)
      (let ((r (funcall (parse-atom) l)))
	(if r r (funcall (parse-list) l)))))

(defun parse-lisp (str)
  "Распознать строку с программой Лисп"
  (parser-value (parse-s) str))
