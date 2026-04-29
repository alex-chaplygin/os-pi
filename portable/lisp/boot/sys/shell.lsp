(defvar *command*) ;; прочитанная команда

(defun is-com-separator (c)
  "Разделители"
  (contains (list (code-char 10) #\ ) c))

(defun get-str ()
  "Получить строку до разделителя или конца потока"
  (parse-app (&&& s->(parse-app (parse-many (parse-pred #'(lambda (x) (not (is-com-separator x))))) #'implode)
		  #'(lambda (x) (if (= s "") nil (cons t x)))) #'car))

(defun parse-elem-str (str)
  "Ожидает ввода строки"
  #'(lambda (stream) (let ((s (funcall (get-str) stream)))
		       (if (= (car s) str) s nil))))

(defmacro parse-command (com fun)
  "Обработка команды"
  `(&&& (parse-elem-str ,com)
	args->(parse-many (parse-app (&&& (parse-many (parse-pred #'is-com-separator))
					  (get-str)) #'second))
	return (progn ,fun 'ok)))

(defun cat-command-action (args)
  "Выполнение команды вывода содержимого файла на экран с аргументами args(путь)"
 ;; (handle
   (let ((file (open-file (car args))))
     (print (arr-get-str (read-file file (File-size file)) 0 (File-size file))))
   ;; (path-not-found (m) (print m))
  ;; (not-file (m) m)))
  )

(defun copy-command-action (args)
  "Выполнение команды копирования файла с аргументами args(путь-откуда путь-куда)"
  (let ((file1 (open-file (car args)))
        (file2 nil))
    (handle
     (create-file (second args))
     (file-exists (message)
                  (progn
                    (list message)
                    (remove-file (second args))
                    (create-file (second args)))))
    (let ((insider (read-file file1 (File-size file1))))
      (setq file2 (open-file (second args)))
      (write-file file2 insider))))

(defun append-command-action (args)
  "Выполнение команды добавление текста в конец файла с аргументами args(путь текст)"
  (let ((file (open-file (car args)))
        (buf (make-array (string-size (second args)))))
    (arr-set-str buf 0 (second args) (string-size (second args)))
    (seek-file file 0 'END)
    (write-file file buf)))

(defun chattr-command-action (args)
  "Выполнение команды изменения атрибутов файла или каталога с аргументами args(путь атрибуты)"
  (let ((path (car args))
        (str-attrs (cdr args))
        (attributes nil))
    (while (not (null str-attrs))
      (setq attributes (append attributes (list (intern (str-toupper (car str-attrs))))))
      (setq str-attrs (cdr str-attrs)))
    (set-attr path attributes)))

(defun parse-commands ()
  "Обработка команд"
  (parse-or (parse-command "clear" (print `(clear ,args)))
	    (parse-command "cwd" (print (cur-dir)))
	    (parse-command "ls" (print (list-dir (cur-dir))))
	    (parse-command "cd" (change-dir (car args)))
	    (parse-command "cat" (cat-command-action args))
	    (parse-command "copy" (copy-command-action args))
	    (parse-command "mkdir" (create-dir (car args)))
	    (parse-command "rm" (remove-file (car args)))
	    (parse-command "rmdir" (remove-dir (car args)))
	    (parse-command "touch" (create-file (car args)))
	    (parse-command "append" (append-command-action args))
	    (parse-command "chattr" (chattr-command-action args))
	    #'(lambda (x) (print "Unknown command"))))

(defun print-start ()
  "Печать приветствия"
  (dolist (x (explode "> ")) (putchar x)))

(defun process-command (com)
  "Обработка команды"
  (let ((c (implode (reverse com))))
    (handle
     (funcall (parse-commands) (stream-from-str c))
     (path-not-found (message)
                     (print "path not found, error message:" message))
     (not-directory (message)
                    (print "not directory, error message:" message))
     (not-file (message)
               (print "not file, error message:" message))
     (read-only (message)
                (print "file is read only, error message:" message))
     (not-enough-space (message)
                       (print "not enough space on disk, error message:" message))
     (end-of-file (message)
                  (print "trying to read end of file, error message:" message))
     (not-empty-dir (message)
                    (print "dir is not empty, error message:" message))
     (argument-error (message)
                     (print "argument error, error message:" message))
     (file-exists (message)
                  (print "file already exists, error message:" message))
     (path-not-found (message)
		     (print "path not found, error message:" message)))
    (print-start)))

;; устанавливаем обработчик нажатия клавиши
(setq *key-down-handler*
      #'(lambda (scan)
	  (let ((c (scan-to-char scan)))
	    (when c
	      (unless (= scan +key-enter+) (setq *command* (cons c *command*)))
	      (putchar c)
	      (when (= scan +key-enter+) (process-command *command*) (setq *command* nil))))))

(load-partition 0 0)
(print-start)
