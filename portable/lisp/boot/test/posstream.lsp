(unit-tests 'postream)

(deftest test-init-pos ()
  "Тест начальной позиции потока"
  (let* ((s (stream-from-str "abc"))
	 (pos (posstream-pos s)))
    (print (assert (pos-line pos) 1))
    (print (assert (pos-char pos) 1))))

(deftest test-init-pos ()
  "Тест чтения символа"
  (let* ((s (stream-from-str "abc"))
	 (s2 (get-byte s))
	 (pos (posstream-pos (cdr s2))))
    (print (assertcar s2 #\a))
    (print (assert (pos-line pos) 1))
    (print (assert (pos-char pos) 2))))

(deftest test-init-pos ()
  "Тест перевода строки"
  (let* ((s (stream-from-str "\nabc"))
	 (s2 (get-byte s))
	 (pos (posstream-pos (cdr s2))))
    (print (assertcar s2 (code-char 10)))
    (print (assert (pos-line pos) 2))
    (print (assert (pos-char pos) 1))))

(run-tests)
