;; *test-failed* - флаг - провалился ли хотя бы один тест.
(defvar *test-failed* nil)

;; Тест
(defun test (target expected-res)
  (unless *test-failed*
    (let ((res (assert target expected-res)))
      (print res)
      (when (eq (car res) 'fail)
	  (setq *test-failed* t)
        ;(print '---------------------)
	  ))))

;; Проверка, все ли тесты успешны
(defun check-tests ()
  (when (null *test-failed*)
    (print "All tests OK")))

(setq *class-table* (make-hash))
(setq *actor-prop-vals* (make-hash))

(print "----------------------------------")
(print "actor-class tests")
;; Создать класс воина
(test (actor-class warrior () (str 20) (dex 15) (int 10))
      'warrior)

;; Создать класс мага
(test (actor-class mage () (str 10) (dex 15) (int 20))
      'mage)

(print "----------------------------------")
(print "make-actor tests")

;; Создать объект воина и проверить характеристики по умолчанию
(test (make-actor warrior)
      '(HASH (CLASS . WARRIOR) (STR . 20) (DEX . 15) (INT . 10)))

;; Создать объект воина с изменёнными характеристиками и проверить их изменение
(test (make-actor warrior (str 30))
      '(HASH (CLASS . WARRIOR) (STR . 30) (DEX . 15) (INT . 10)))

;; Создать объект мага с новыми характеристиками(одной) и проверить их добавление
(test (make-actor mage (mp 30))
      '(HASH (CLASS . MAGE) (STR . 10) (DEX . 15) (INT . 20) (MP . 30)))


;(check-tests)
