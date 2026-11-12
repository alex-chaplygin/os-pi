(defvar *terminal-buffer*)

(defun terminal-expression-eval(line)
  "Разбор вычисления, его вычисление"
  (handle
   (let ((res (funcall (lisp-s-expr)
                       (stream-from-list (lisp-lexer line)))))
     (cond ((null res)
            (print "invalid expression"))
           ((get-byte (cdr res))
            (print "more than one expression"))
           (t
            (print (eval (token-value (car res)))))))
   (parse-error (message)
                (print "parse invalid, error message:" message))))

(defun terminal-key-input (scan)
  "Регистрирование нажатия кнопок клавиатуры"
  (let ((c (if (or (key-pressed +key-left-shift+) (key-pressed +key-right-shift+))
               (scan-to-shift-char scan)
               (scan-to-char scan))))
    (when c
      (putchar c)
      (if (= scan +key-enter+)
          (progn
            (terminal-expression-eval (implode (reverse *terminal-buffer*)))
            (setq *terminal-buffer* nil)
            (putstring "> "))
          (setq *terminal-buffer* (cons c *terminal-buffer*))))))

(defun terminal ()
  "Запуск терминала"
  (setq *terminal-buffer* nil)
  (setq *key-down-handler* #'terminal-key-input)
  (putstring "> "))

(terminal)
