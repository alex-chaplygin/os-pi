(defun null (x)
  (eq x (quote())))

(defun fac(x)
  (cond
    ((= x 1) 1)
    (t (* x (fac (- x 1))))))
