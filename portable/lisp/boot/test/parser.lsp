(defun parser-tests ()
  (let ((parse-4-digit (parse-element 4))
	(parse-1-digit (parse-element 1)))
    (print (assert (funcall parse-4-digit '(4 5 6)) '((4 5 6))))
    (print (assert (funcall parse-4-digit '(1 2 3)) '((() 1 2 3))))
    (print (assert (funcall parse-1-digit '(4 5 6)) '((() 4 5 6))))
    (print (assert (funcall parse-1-digit '(1 2 3)) '((1 2 3))))))

(parser-tests)
