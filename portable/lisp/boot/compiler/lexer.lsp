;; Вспомогательный предикат
(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\%) sym))

;; Простые парсеры атомов (не зависят от parse-s / parse-list)

(defun parse-char (char)
  "Разбор символа char с учетом пробелов"
  (parse-app
    (&&& (skip-spaces) (parse-elem char))
    #'second))

(defun parse-hex ()
  "Разбор шестнадцатеричного числа вида 0xFF"
  (parse-app
    (&&& (skip-spaces)
         (parse-char #\0)
         (parse-char #\x)
         (parse-pred #'is-hex-sym)
         (parse-many (parse-pred #'is-hex-sym)))
    #'(lambda (parts)
        (strtoint (implode (cons (fourth parts) (fifth parts))) 16))))

(defun parse-decimal ()
  "Разбор десятичного числа (поддерживает -123)"
  (parse-app
    (&&& (skip-spaces)
         (parse-optional (parse-char #\-))   ; ← необязательный минус
         (parse-pred #'is-digit)
         (parse-many (parse-pred #'is-digit)))
    #'(lambda (parts)
        (let ((minus (second parts))        ; nil или #\-
              (first-digit (third parts))
              (rest-digits (fourth parts)))
          (let ((num (strtoint (implode (cons first-digit rest-digits)) 10)))
            (if minus (- num) num))))))

(defun parse-tnumber ()
  "Разбор десятичного (возможно, отрицательного) или шестнадцатеричного числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun parse-tsymbol ()
  "Разбор символа (идентификатора)"
  (parse-app
    (&&& (skip-spaces)
         (parse-or (parse-pred #'is-alpha)
                   (parse-pred #'is-lisp-symbol))
         (parse-many (parse-or (parse-pred #'is-alpha)
                               (parse-pred #'is-digit)
                               (parse-pred #'is-lisp-symbol))))
    #'(lambda (parts)
        (intern (implode (map #'(lambda (char) (toupper char))
                              (cons (second parts) (third parts))))))))

(defun parse-tchar ()
  "Разбор символа вида #\\a"
  (parse-app
    (&&& (skip-spaces)
         (parse-char #\#)
         (parse-char #\\)
         (parse-pred #'(lambda (sym) t)))
    #'fourth))

(defun parse-tfunction ()
  "Разбор функции вида #'name"
  (parse-app
    (&&& (skip-spaces)
         (parse-char #\#)
         (parse-char #\')
         (parse-s))  ; ← зависит от parse-s (циклическая зависимость — допустима)
    #'(lambda (parts) (list 'FUNCTION (fourth parts)))))

(defun parse-escape (char value)
  "Разбор экранированной последовательности"
  (parse-app
    (&&& (parse-char #\\) (parse-char char))
    #'(lambda (parts) (list value))))

(defun parse-tstring ()
  "Разбор строки в двойных кавычках"
  (parse-app
    (&&& (skip-spaces)
         (parse-char #\")
         (parse-many (parse-or (parse-escape #\n (code-char 0xa))
                               (parse-pred #'(lambda (sym) (!= sym #\")))))
         (parse-char #\"))
    #'(lambda (parts) (implode (third parts)))))

(defun parse-tarray ()
  "Разбор массива #(...)"
  (parse-app
    (&&& (parse-char #\#) (parse-list))
    #'(lambda (parts) (list-to-array (second parts)))))

;; Составные парсеры (зависят от атомов и друг от друга)

(defun parse-atom ()
  "Разбор атома: число, символ, строка, #..., #'..."
  #'(lambda (str)
      (let ((res (funcall (parse-or (parse-tnumber)
                                    (parse-tsymbol)
                                    (parse-tarray)
                                    (parse-tchar)
                                    (parse-tfunction)
                                    (parse-tstring)) str)))
        (if res (list (car res)) nil))))

(defun parse-list ()
  "Разбор списка: ( ... )"
  (parse-app
    (&&& (parse-char #\()
         (parse-many (parse-s))
         (parse-char #\)))
    #'second))

(defun parse-s ()
  "Разбор s-выражения: атом или список"
  (parse-or (parse-atom) (parse-list)))


(defun parse-lisp (str)
  "Полный разбор Lisp-выражения из строки"
  (caar (funcall (parse-s) (explode str))))