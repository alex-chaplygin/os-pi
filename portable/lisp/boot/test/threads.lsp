(defun A (p)
  (for i 0 10
       (print p i)
       (yield)))

(fork #'A "a")
(fork #'A "b")
(fork #'A "c")
(thread-exit)
