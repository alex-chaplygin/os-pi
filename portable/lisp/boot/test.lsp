(defvar *test-list*) ; глобальный список имен тестов
(defconst +epsilon+ 0.000001)

(defun assert (res res2)
  "Проверка на равенство результата res и эталона res2"
  (if (equal res res2) `(OK ,res ,res2) `(FAIL ,res ,res2)))

(defun assert-float (res res2)
  "Проверка на равенство вещественных значений res и эталона res2"
  (if (< (abs (- res res2)) +epsilon+) `(OK ,res ,res2) `(FAIL ,res ,res2)))

(defun unit-tests (name)
  "Новый драйвер тестов с именем name"
  (setq *test-list* nil))

(defmacro deftest (name doc params &rest body)
  "Создать тест с именем name документацией doc и телом body"
  `(defun ,name ()
     (print ,doc)
     ,@body)
  `(setq *test-list* (append *test-list* (list #',name))))

(defun run-tests (&rest unit)
  "Запуск всех тестов в модуле unit"
  (app #'(lambda (n) (funcall n)) *test-list*))
