(defvar *command*) ;; прочитанная команда

(defun is-com-separator (c)
  "Разделители"
  (contains (list (code-char 10) #\ ) c))

(defun get-str ()
  "Получить строку до разделителя"
  (parse-app (parse-many (parse-pred #'(lambda (x) (not (is-com-separator x))))) #'implode))

(defun parse-elem-str (str)
  "Ожидает ввода строки"
  #'(lambda (stream) (let ((s (funcall (get-str) stream)))
		       (if (= (car s) str) s nil))))

(defmacro parse-command (com fun)
  "Обработка команды"
  `(&&& (parse-elem-str ,com)
	args->(parse-many-n 4 (parse-app (&&& (parse-many (parse-pred #'is-com-separator))
					      (get-str)) #'second))
	return (progn ,fun 'ok)))

(defun parse-commands ()
  "Обработка команд"
  (parse-or (parse-command "clear" (print `(clear ,args)))
	    (parse-command "ls" (print `(ls ,args)))
	    #'(lambda (x) (print "Unknown command"))))

(defun print-start ()
  "Печать приветствия"
  (dolist (x (explode "> ")) (putchar x)))

(defun process-command (com)
  "Обработка команды"
  (let ((c (implode (reverse com))))
    (funcall (parse-commands) (stream-from-str c))
    (print-start)))

;; устанавливаем обработчик нажатия клавиши
(setq *key-down-handler*
      #'(lambda (scan)
	  (let ((c (scan-to-char scan)))
	    (when c
	      (unless (= scan +key-enter+) (setq *command* (cons c *command*)))
	      (putchar c)
	      (when (= scan +key-enter+) (process-command *command*) (setq *command* nil))))))

(print-start)
