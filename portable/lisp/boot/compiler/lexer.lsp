;; Вспомогательный предикат
(defun is-lisp-symbol (sym)
  "Предикат проверки на особый символ"
  (contains '(#\+ #\- #\* #\/ #\= #\_ #\& #\| #\< #\> #\%) sym))

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

;; (defun parse-tchar ()
;;   "Разбор символа вида #\\a"
;;   (parse-app
;;     (&&& (skip-spaces)
;;          (parse-elem #\#)
;;          (parse-elem #\\)
;;          (parse-pred #'(lambda (sym) t)))
;;     #'fourth))

;; (defun parse-tfunction ()
;;   "Разбор функции вида #'name"
;;   (parse-app
;;     (&&& (skip-spaces)
;;          (parse-elem #\#)
;;          (parse-elem #\')
;;          (parse-s))  ; ← зависит от parse-s (циклическая зависимость — допустима)
;;     #'(lambda (parts) (list 'FUNCTION (fourth parts)))))

;; (defun parse-escape (char value)
;;   "Разбор экранированной последовательности"
;;   (parse-app
;;     (&&& (parse-elem #\\) (parse-elem char))
;;     #'(lambda (parts) (list value))))

;; (defun parse-tstring ()
;;   "Разбор строки в двойных кавычках"
;;   (parse-app
;;     (&&& (skip-spaces)
;;          (parse-elem #\")
;;          (parse-many (parse-or (parse-escape #\n (code-char 0xa))
;;                                (parse-pred #'(lambda (sym) (!= sym #\")))))
;;          (parse-elem #\"))
;;     #'(lambda (parts) (implode (third parts)))))

;; (defun parse-tarray ()
;;   "Разбор массива #(...)"
;;   (parse-app
;;     (&&& (parse-elem #\#) (parse-list))
;;     #'(lambda (parts) (list-to-array (second parts)))))
