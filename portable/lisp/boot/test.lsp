(defvar *test-list*) ; глобальный список имен тестов
(defconst +epsilon+ 0.000001)

(defun assert (res res2)
  "Проверка на равенство результата res и эталона res2"
  (if (equal res res2) `(OK ,res ,res2) `(FAIL ,res ,res2)))

(defun assert-float (res res2)
  "Проверка на равенство вещественных значений res и эталона res2"
  (if (< (abs (- res res2)) +epsilon+) `(OK ,res ,res2) `(FAIL ,res ,res2)))

(defmacro deftest (name &rest body)
  "Создать тест с именем name и телом body"
  `(defun ,name ()
     ,@body))
