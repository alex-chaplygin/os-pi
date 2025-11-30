(defvar *events* (queue-create)) ;создание очереди событий

(defun events-loop ()
  "Глобальный цикл обработки событий"
  (while t
;;	 (setq *e* (queue-pop *events*))
;;	 (if (eq *e* nil) nil
  ;;  	     (print *e*))))
    ))

;;(setq *key-down-handler* #'(lambda (key) (print `(keydown ,key))))
;;(setq *key-up-handler* #'(lambda (key) (print `(keyup ,key))))
