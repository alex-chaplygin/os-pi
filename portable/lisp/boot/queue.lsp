;Очередь представленна как список с первым элементом QUEUE

(defun queue-create ()
  "Конструктор очереди, возвращает обьект очереди"
  (list 'queue))

(defun queue-push (qu el)
  "Добавление элемента в очередь"
  "qu - очередь в которую нужно добавить элемент"
  "el - элемент который нужно добавить"
  (labels ((last-pair (list)
		      (if (null (cdr list)) list
			(last-pair (cdr list)))))
	  (if (equal (car qu) 'queue) (rplacd (last-pair qu) (list el))
	    (error "queue-push: not a queue"))))

(defun queue-pop (qu)
  "Удаляет первый элемент из очереди и возвращает егоба если очередь пустая возвращает nil"
  "qu - очередь из оторой нужно удалить элемент"
  )
