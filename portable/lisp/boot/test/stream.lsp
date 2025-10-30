;; Модульные тесты потоков
(unit-tests 'stream)

(deftest get-byte-test ()
  "Тестирование чтения байт"
  (let* ((s (stream-from-arr #(255 132 122) nil))
	 (b1 (get-byte s))
	 (b2 (get-byte (cdr b1)))
	 (b3 (get-byte (cdr b2))))
  (print (assertcar b1 255))
  (print (assertcar b2 132))
  (print (assertcar b3 122))))

(deftest get-word-test ()
  "Тестирование чтения слова"
  (let* ((s (stream-from-arr #(0x10 0xff 0xcc 0xcb) t))
	 (w (get-word s)))
    (print (assertcar w 0x10ff))
    (astream-set-endianness (cdr w) nil) ; little-endian
    (print (assertcar (get-word (cdr w)) 0xcbcc))))

(deftest get-dword-test ()
  "Тестирование чтения слова 32-бит"
  (let* ((s (stream-from-arr #(0x10 0xff 0xcc 0xcb 0x10 0xff 0xcc 0xcb) t))
	 (w (get-dword s)))
    (print (assertcar w 0x10ffcccb))
    (astream-set-endianness (cdr w) nil) ; little-endian
    (print (assertcar (get-dword (cdr w)) 0xcbccff10))))

(deftest get-4bit-test ()
  "Тестирование чтения 4 бит"
  (let* ((s (stream-from-arr #(0x67 0xff) t))
	 (b (get-4bit s)))
    (print (assertcar b '(6 . 7)))
    (print (assertcar (get-4bit (cdr b)) '(15 . 15)))))

(deftest get-array-test ()
  "Тестирование чтения массива"
  (let* ((s (stream-from-arr #(0x16 0xff 0xa0) t))
	 (a1 (get-array s 2))
	 (a2 (get-array (cdr a1) 1)))
    (print (assertcar a1 #(22 255)))
    (print (assertcar a2 #(160)))))
	 
(run-tests)
