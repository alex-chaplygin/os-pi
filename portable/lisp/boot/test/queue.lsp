(setq q (queue-create))
(setq d (queue-pop q))
(assert d nil)
(queue-push q 1)
(queue-push q 2)
(queue-push q 3)
(assert q '(queue 1 2 3))
(queue-push (list 1 2 3) 3)
(let ((a (queue-pop q))
      (b (queue-pop q))
      (c (queue-pop q))
      (d (queue-pop q)))
  (print (assert a 1))
  (print(assert b 2))
  (print(assert c 3))
  (assert d nil))

(defun empty-queue-tests ()
  (print "Создание пустой очереди и попытка извлечь элемент")
  (let ((q (queue-create)))
    (print (assert (queue-pop q) nil))))

(empty-queue-tests)
