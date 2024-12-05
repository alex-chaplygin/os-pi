"--------------------TEST--------------------"
(let ((temp (make-tree (make-leaf 4) (make-tree (make-leaf 6) (make-leaf nil) nil) 5))
      (low (make-tree (make-leaf 1) (make-leaf 2) 3)))
  (print (assert (is-leaf temp) nil)) ;Проверка на лист для узла
  (print (assert (correct-tree temp) t)) ;Проверка корректности узла
  (print (assert (is-leaf (make-leaf 4)) t)) ;Проверка на лист для листа
  (print (assert (left-tree temp) (make-leaf 4))) ;Проверка получения левого поддерева
  (print (assert (right-tree temp) (make-tree (make-leaf 6) (make-leaf nil) nil))) ;Проверка получения правого поддерева
  (print (assert (left-tree (right-tree temp)) (make-leaf 6))) ;Проверка получения правого поддерева
  (print (assert (tree-get-val low) 3)) ;Проверка получения значения узла
  (assert (tree-get-val (left-tree low)) 1)) ;Проверка получения значения листа


