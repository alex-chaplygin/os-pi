(defun parse-tarray ()
  "Разбор массива"
  (&&& #'(lambda (l) (list-to-array (second l)))
       (parse-char #\#)
       (parse-list)))

(defun parse-char (char)
  "Разбор символа char с учетом пробелов"
  (&&& #'second (skip-spaces) (parse-elem char)))

(defun parse-decimal ()
  "Разбор десятичного числа"
  (&&& #'(lambda (l) (strtoint (implode (cons (second l) (third l))) 10))
       (skip-spaces)
       (parse-pred #'is-digit)
       (parse-many (parse-pred #'is-digit))))

(defun parse-hex ()
  "Разбор шестнадцатеричного числа"
  (&&& #'(lambda (l) (strtoint (implode (cons (forth l) (fifth l))) 16))
       (skip-spaces)
       (parse-char #\0)
       (parse-char #\x)
       (parse-pred #'is-hex-sym)
       (parse-many (parse-pred #'is-hex-sym))))

(defun parse-tnumber ()
  "Разбор десятичного или шестнадцатеричного числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\%) sym))

(defun parse-tsymbol ()
  "Разбор символа"
  (&&& #'(lambda (l) (intern (implode (map #'(lambda (char) (toupper char)) (cons (second l) (third l))))))
       (skip-spaces)
       (parse-or (parse-pred #'is-alpha)
                 (parse-pred #'is-lisp-symbol))
       (parse-many (parse-or (parse-pred #'is-alpha)
			     (parse-pred #'is-digit)
			     (parse-pred #'is-lisp-symbol)))))

(defun parse-tchar ()
  "Разбор lisp-символа #\\"
  (&&& #'(lambda (l) (forth l))
       (skip-spaces)
       (parse-char #\#)
       (parse-char #\\)
       (parse-pred #'(lambda (sym) t))))

(defun parse-tfunction ()
  "Разбор символа функции '#"
  (&&& #'(lambda (l) (list 'FUNCTION (forth l)))
       (skip-spaces)
       (parse-char #\#)
       (parse-char #\')
       (parse-s)))

(defun parse-escape (char value)
  "Разбор экранирования"
  (&&& #'(lambda (l) (list value))
       (parse-char #\\)
       (parse-char char)))

(defun parse-tstring ()
  "Разбор строки"
  (&&& #'(lambda (l) (implode (third l)))
       (skip-spaces)
       (parse-char #\")
       (parse-many (parse-or (parse-escape #\n (code-char 0xa))
                             (parse-pred #'(lambda (sym) (!= sym #\")))))
       (parse-char #\")))

(defun parse-atom ()
  "Разбор атома, начинается с буквы, содержит хотя бы одну букву."
  #'(lambda (str)
      (let ((res (funcall (parse-or (parse-tnumber)
				    (parse-tsymbol)
				    (parse-tarray)
				    (parse-tchar)
				    (parse-tfunction)
				    (parse-tstring)) str)))
        (if res (list (car res))
	    nil))))

(defun parse-list ()
  "Разбор списка s-выражений"
  #'(lambda (str)
      (let ((res (funcall (&&& #'second
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
