(defvar *test-list*) ; глобальный список имен тестов
(defvar *test-started-count* 0) ; количество запущенных тестов
(defvar *test-ok-count* 0) ; количество успешных тестов
(defconst +epsilon+ 0.000001)

(defun assert (res res2)
  "Проверка на равенство результата res и эталона res2"
  (incf *test-started-count*)
;;  (print "Тест №" *test-started-count*)
  (if (equal res res2) (progn (incf *test-ok-count*) `(OK ,res ,res2)) `(FAIL ,res ,res2)))

(defun assertcar (res res2)
  "Проверка на равенство car результата res и эталона res2"
  (assert (car res) res2))

(defun assert-float (res res2)
  "Проверка на равенство вещественных значений res и эталона res2"
  (incf *test-started-count*)
;;  (print "Тест №" *test-started-count*)
  (if (< (abs (- res res2)) +epsilon+) (progn (incf *test-ok-count*) `(OK ,res ,res2)) `(FAIL ,res ,res2)))

(defun unit-tests (name)
  "Новый драйвер тестов с именем name"
  (setq *test-list* nil
        *test-started-count* 0
        *test-ok-count* 0))

(defmacro deftest (name params doc &rest body)
  "Создать тест с именем name документацией doc и телом body"
  `(defun ,name ()
     (print ,doc)
     ,@body)
  `(setq *test-list* (append *test-list* (list #',name))))

(defun run-tests (&rest options)
  "Запуск всех тестов в данном модуле"
  (print)
  (let ((stop-on-fail (contains options 'stop-on-fail)))
    (app #'(lambda (test)
             (when (or (not stop-on-fail)
                       (= *test-started-count* *test-ok-count*))
               (funcall test)))
         *test-list*))
  (when (> *test-started-count* 0)
    (print "Успешно завершено тестов: " *test-ok-count* '/ *test-started-count*)
    (when (= *test-started-count* *test-ok-count*)
      (print "Все тесты завершены успешно"))))
