;; Реализация зеленых потоков через продолжения

;; Список потоков, каждый является объектом-продолжением
(defvar *threads*) 

(defun thread-exit ()
  "Остановка текущего потока"
  (print `(threads ,*threads*))
  (when *threads*
    (let ((cont (car *threads*)))
      (setq *threads* (cdr *threads*))
      (if (functionp cont) (funcall cont nil) (call-continuation cont nil)))))

(defun fork(fn &rest args)
  "Запустить функцию fn в новом потоке с аргументами args"
  "Поток принудительно остановится когда функция завершится"
  (print `(fork ,fn ,args))
  (setq *threads* (append *threads* (list #'(lambda (x)
					      (apply fn args)
					      (thread-exit))))))

(defun yield ()
  "Передача управления на следующий поток"
  (call/cc #'(lambda (cont)
	       (setq *threads* (append *threads* (list cont)))
	       (thread-exit)))
  )

					  
