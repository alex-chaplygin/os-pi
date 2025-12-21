(defun A ()
  (for i 0 10
       (print "a" i)
       (yield)))

(defun B ()
  (for i 0 10
       (print "b" i)
       (yield)))

(fork #'A)
(fork #'B)
(print *threads*)
(thread-exit)
