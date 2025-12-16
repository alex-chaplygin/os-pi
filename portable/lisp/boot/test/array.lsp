(unit-tests 'array)

(deftest test-array-cat ()
  "Конкатенация массивов"
  (print (assert (array-cat #(1 2 3) #(4 5 6)) #(1 2 3 4 5 6))))

(deftest test-array-seq (x y arr res)
  "Срез массива"
  (print (assert (array-seq #(0 1 2 3 4 5 6 7 8 9) 2 6) #(2 3 4 5))))

(defun test-arr-set-str (arr ofs str size)
    (arr-set-str arr ofs str size)
    (print (assert arr #(0 0 0x61 0x62 0x63))))

(test-arr-set-str #(0 0 0 0 0) 2 "abc" 3)

(defun test-arr-get-str (arr ofs str size)
    (arr-set-str arr ofs str size)
    (print (assert (arr-get-str arr ofs size) str)))

(test-arr-get-str #(0 0 0 0 0) 2 "abc" 3)

(defun test-arr-set-num (arr ofs val size)
    (arr-set-num arr ofs val size)
    (print (assert arr #(0 0 0x30 0x31))))

(test-arr-set-num #(0 0 0 0) 2 0x3130 2)

(defun test-arr-get-num (arr ofs size)
  (print (assert (arr-get-num arr ofs size) 0x3130)))

(defun test-array-to-list (arr list)
  (print (assert (array-to-list arr) list)))

(test-arr-get-num #(0 0 0x30 0x31 0x32 0x33) 2 2)

(deftest ar-to-list ()
  "Массив в список"
  (test-array-to-list #() ())
  (test-array-to-list #(1) '(1))
  (test-array-to-list #(1 2 3 4 5) '(1 2 3 4 5)))

(deftest ar-sum ()
  "Сумма элементов массива"
  (print (assert (array-sum #(1 2 3)) 6))
  (print (assert (array-sum #()) 0)))

(deftest ar-max ()
  "Максимальный элемент массива"
  (print (assert (array-max #(1 2 3)) 3))
  (print (assert (array-max #(4 2 1 0 -5)) 4)))

(run-tests)
