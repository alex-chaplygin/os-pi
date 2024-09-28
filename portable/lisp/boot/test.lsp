(defvar *test-list*) ; глобальный список имен тестов

(defun assert (res res2)
  "Проверка на равенство результата res и эталона res2"
  (if (equal res res2) `(OK ,res ,res2) `(FAIL ,res ,res2)))

(defmacro deftest (name &rest body)
  "Создать тест с именем name и телом body"
  `(defun ,name ()
     ,@body))
