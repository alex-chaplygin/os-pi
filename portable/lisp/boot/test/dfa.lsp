; Тесты детерминированного автомата
(print "-----DFA tests-----")

(defun check(auto tape reference)
  (let ((result (foldl #'dfa-input auto tape)))
    (print (assert (dfa-end result) reference))))

(defvar auto1 (make-dfa 's1            ;начальное состояние
		      '((s1 a s2)    ;список из 3 правил
			(s1 b s3)
			(s3 c s2))
		      '(s2)))        ;конечные состояния, при к-рых автомат вовр. true

(check auto1 '(a) T)
(check auto1 '(b c) T)
(check auto1 '(b) NIL)
(check auto1 '(b b) NIL)
(check auto1 '(b c a) NIL)
(check auto1 '(d q z) NIL)

(defvar auto2 (make-dfa 's1
			'((s1 x s4)
			  (s1 y s1)
			  (s1 z s2)
			  (s2 y s3)
			  (s3 x s1)
			  (s4 x s3)
			  (s4 y s3)
			  (s4 z s4))
			'(s1 s2)))

(check auto2 '(y) T)
(check auto2 '(y y y) T)
(check auto2 '(x x) NIL)
(check auto2 '(x x x) T)
(check auto2 '(z x) NIL)
(check auto2 '(z y x) T)
(check auto2 '(x z z) NIL)
(check auto2 '(x z z y x) T)

(print (assert (dfa-state (dfa-reset auto2 's9)) 's9))

