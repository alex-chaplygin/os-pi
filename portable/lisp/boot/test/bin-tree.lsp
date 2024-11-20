"--------------------TEST--------------------"
(let ((temp (make-tree (make-leaf 4) (make-tree (make-leaf 6) (make-leaf nil) nil) 5))
      (low (make-tree (make-leaf 1) (make-leaf 2) 3)))
  (print (assert (is-leaf temp) nil))
  (print (assert (is-leaf (make-leaf 4)) t))
  (print (assert (left-tree temp) (make-leaf 4)))
  (print (assert (right-tree temp) (make-tree (make-leaf 6) (make-leaf nil) nil)))
  (print (assert (left-tree (right-tree temp)) (make-leaf 6)))
  (print (assert (set-left-tree low (make-leaf 5)) (make-tree (make-leaf 5) (make-leaf 2) 3)))
  (print (assert (set-right-tree low (make-leaf 6)) (make-tree (make-leaf 5) (make-leaf 6) 3)))
  (print (assert (tree-get-val low) 3))
  (assert (tree-set-val low 10) (make-tree (make-leaf 5) (make-leaf 6) 10)))


