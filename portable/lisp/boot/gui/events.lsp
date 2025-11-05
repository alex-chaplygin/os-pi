(defvar *events* (queue-create)) ;создание очереди событий

(defun key-queue-handler ()
  "Обработчик прерывания клавиатуры"
  (let ((status (inb +KEY-STATUS+))) ; получает статус, есть ли данные в буфере клавиатуры
    (when (equal (& status 1) 1) ; если есть (младший бит регистра статуса)
      (let ((scan (inb +KEY-BUFFER+))) ; читаем скан код из буфера
	(if (< scan 128)
	    (progn  ; если меньше 128, то это нажатие клавиши
	      (queue-push *events* (cons 'keydown scan))
	      (print *events*))
		   ; иначе это отпускание клавиши
	    (queue-push *events* (cons 'keyup (- scan 128))))))))

(defun events-loop ()
  "Глобальный цикл обработки событий"
  (set-int-handler +key-irq+ #'key-queue-handler)
  (while t
;;	 (setq *e* (queue-pop *events*))
;;	 (if (eq *e* nil) nil
  ;;  	     (print *e*))))
    ))
