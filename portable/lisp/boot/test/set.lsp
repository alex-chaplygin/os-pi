(unit-tests 'set)

(deftest make-set-test ()
  "Тесты для make-set"
  (print (assert (empty-set (make-set)) t))
  (print (assert (set-size (make-set)) 0))
  (print (assert (set-member 1 (make-set)) nil)))

(deftest empty-set-test ()
  "Тесты для empty-set"
  (let ((empty (make-set))
        (non-empty (set-insert (make-set) 1)))
    (print (assert (empty-set empty) t))
    (print (assert (empty-set non-empty) nil))))

(deftest set-member-test ()
  "Тесты для set-member"
  (let ((set (set-insert (set-insert (make-set) 1) 2)))
    (print (assert (set-member 1 set) t))
    (print (assert (set-member 2 set) t))
    (print (assert (set-member 3 set) nil))
    (print (assert (set-member 0 set) nil))))

(deftest set-insert-test ()
  "Тесты для set-insert"
  (let* ((empty (make-set))
         (s1 (set-insert empty 1))
         (s2 (set-insert s1 2))
         (s3 (set-insert s2 1))) ; попытка добавить дубликат
    (print (assert (set-size s1) 1))
    (print (assert (set-size s2) 2))
    (print (assert (set-size s3) 2)) ; размер не изменился
    (print (assert (set-member 1 s3) t))
    (print (assert (set-member 2 s3) t))))

(deftest set-remove-test ()
  "Тесты для set-remove"
  (let* ((set (set-insert (set-insert (make-set) 1) 2))
         (set1 (set-remove 1 set))
         (set2 (set-remove 3 set1))) ; удаление несуществующего элемента
    (print (assert (set-size set) 2))
    (print (assert (set-size set1) 1))
    (print (assert (set-size set2) 1)) ; размер не изменился
    (print (assert (set-member 1 set1) nil))
    (print (assert (set-member 2 set1) t))))

(deftest set-union-test ()
  "Тесты для set-union"
  (let* ((set1 (set-insert (set-insert (make-set) 1) 2))
         (set2 (set-insert (set-insert (make-set) 2) 3))
         (union (set-union set1 set2)))
    (print (assert (set-size union) 3))
    (print (assert (set-member 1 union) t))
    (print (assert (set-member 2 union) t))
    (print (assert (set-member 3 union) t))
    (print (assert (set-member 4 union) nil))))

(deftest set-intersect-test ()
  "Тесты для set-intersect"
  (let* ((set1 (set-insert (set-insert (make-set) 1) 2))
         (set2 (set-insert (set-insert (make-set) 2) 3))
         (intersect (set-intersect set1 set2)))
    (print (assert (set-size intersect) 1))
    (print (assert (set-member 2 intersect) t))
    (print (assert (set-member 1 intersect) nil))
    (print (assert (set-member 3 intersect) nil))))

(deftest list-to-set-test ()
  "Тесты для list-to-set"
  (let* ((lst1 '(1 2 3 4 5))
         (lst2 '(1 2 2 3 3 3 4))
         (set1 (list-to-set lst1))
         (set2 (list-to-set lst2)))
    (print (assert (set-size set1) 5))
    (print (assert (set-size set2) 4)) ; дубликаты удалены
    (print (assert (set-member 1 set2) t))
    (print (assert (set-member 2 set2) t))
    (print (assert (set-member 3 set2) t))
    (print (assert (set-member 4 set2) t))
    (print (assert (set-member 5 set2) nil))))

(deftest set-size-test ()
  "Тесты для set-size"
  (let ((empty (make-set))
        (single (set-insert (make-set) 1))
        (multiple (set-insert (set-insert (set-insert (make-set) 1) 2) 3)))
    (print (assert (set-size empty) 0))
    (print (assert (set-size single) 1))
    (print (assert (set-size multiple) 3))))

(run-tests)
