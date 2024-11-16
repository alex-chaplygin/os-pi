(defvar *events* (queue-create)) ;создание очереди событий

(defun events-loop ()
  "Глобальный цикл обработки событий"
  (while t
	 (key-handler)
	 (setq *e* (queue-pop *events*))
	 (if (eq *e* nil) nil
	     (print *e*))))
	  
(events-loop)
