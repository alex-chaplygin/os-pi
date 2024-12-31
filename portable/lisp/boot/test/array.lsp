(defun test-array-cat (x y)
    (print (assert (array-cat x y) #(1 2 3 4 5 6))))

(test-array-cat #(1 2 3) #(4 5 6))

(defun test-array-seq (x y arr res)
    (print (assert (array-seq arr x y) res)))

(test-array-seq 2 6 #(0 1 2 3 4 5 6 7 8 9) #(2 3 4 5))

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

(test-arr-get-num #(0 0 0x30 0x31 0x32 0x33) 2 2)
