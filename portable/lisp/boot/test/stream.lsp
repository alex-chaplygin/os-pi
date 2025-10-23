;; Модульные тесты потоков
(unit-tests 'stream)

(deftest get-byte-test ()
  "Тестирование чтения байт"
  (print "get-byte-test")
  (let* ((s (stream-from-arr #(255 132 122) nil))
	 (b1 (get-byte s))
	 (b2 (get-byte (cdr b1)))
	 (b3 (get-byte (cdr b2))))
  (print (assertcar b1 255))
  (print (assertcar b2 132))
  (print (assertcar b3 122))))

(deftest get-word-test ()
  "Тестирование чтения слова"
  (print "get-word-test")
  (let* ((s (stream-from-arr #(0x10 0xff 0xcc 0xcb) t))
	 (w (get-word s)))
    (print (assertcar w 0x10ff))
    (astream-set-endianness (cdr w) nil) ; little-endian
    (print (assertcar (get-word (cdr w)) 0xcbcc))))

(run-tests)
