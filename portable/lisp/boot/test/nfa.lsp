; Тесты недетерминированного автомата
(print "-----NFA tests-----")

(defun check(auto tape reference)
  (let ((result (foldl #'nfa-input auto tape)))
    (print (assert (nfa-end result) reference))))

(defun debug(auto tape &rest null)
  (print `(test ,auto))
  (print `(result = ,(nfa-end (foldl #'(lambda (a s)
					 (print `(states ,(nfa-states a) sym ,s))
					 (nfa-input a s))
				     auto tape)))))

(defvar auto1 (make-nfa '(s0)
			'(
			  (s0 a (s1))
			  (s2 c (s2))
			  (s3 c (s3 s0 s1))
			  (s1 a (s2))
			  (s3 b (s2))
			  (s0 b (s3 s0))
			 )
			'(s2)))

(check auto1 '(a a c) T)
(check auto1 '(c a b) NIL)
(check auto1 '(b c c c b) T)
(check auto1 '(b c c a) T)
(check auto1 '(b c b b a a c c) T)
(check auto1 '() NIL)

(defvar auto2 (make-nfa '(s0)
			'(
			  (s0 x (s3))
			  (s0 x (s2))
			  (s0 x (s1))
			  (s0 a (s4 s5))
			  (s0 y (s6))
			  (s3 y (s3 s6))
			  (s3 x (s1))
			  (s4 b (s2))
			  (s2 x (s1))
			  (s2 y (s6))
			  (s6 y (s6))
			  (s5 a (s5))
			  (s5 x (s1))
			  (s5 y (s6))
			 )
			'(s1 s6)))

(check auto2 '(x x) T)
(check auto2 '(x x x) NIL)
(check auto2 '(x y y y y x) T)
(check auto2 '(a a a y y) T)
(check auto2 '(x y x) T)
(check auto2 '(y x) NIL)
(check auto2 '(y y x) NIL)


(defvar auto3 (make-nfa '(0)
			'(
			  (0 #\a (1))
			  (1 #\b (2))
			  (2 ANY (3))
			  (3 #\d (4))
			 )
			'(4)))

(check auto3 (explode "abcd") T)
(check auto3 (explode "abqd") T)
(check auto3 (explode "ab d") T)
(check auto3 (explode "abd") NIL)
(check auto3 (explode "aabcd") NIL)


(print (assert (nfa-states (nfa-reset auto2 '(s3 s5))) '(s3 s5)))
(print (assert (nfa-states (nfa-reset auto3 '(7))) '(7)))
(print (assert (nfa-states (nfa-reset auto3 '())) NIL))

(defvar auto4 (make-nfa '(0)
			'(
			  (0 ANY (0))
			  (0 #\a (1))
			  (0 #\a (2))
			  (0 E   (2))
			  (2 #\x (1))
			 )
			'(1)))

(check auto4 '(#\r #\r #\r #\x) T)
(check auto4 '(#\x) T)

(defvar auto5 (make-nfa '(0)
			'(
			  (0 a (9))
			  (9 E (1))
			  (1 E (2))
			  (2 c (4))
			  (2 x (6))
			  (6 E (7))
			  (7 y (2))
			  (2 E (3))
			  (3 b (4))
		         )
			'(4)))

(check auto5 '(a b) T)
(check auto5 '(a c) T)
(check auto5 '(a d b) NIL)
(check auto5 '(a x y b) T)
(check auto5 '(a x y c) T)
(check auto5 '(a x y) NIL)

(defvar auto6 (make-nfa '(0)
			'(
			  (0 a (1))
			  (0 E (2))
			  (2 E (3))
			  (3 E (4))
		         )
			'(1 4)))

(print (assert (nfa-end auto6) T))
(check auto6 '(b) NIL)

;(defvar auto7 (make-nfa '(0)
;			'(
;			  (0 a (1))
;			  (1 E (2))
;			  (2 E (1))
;			  (2 x (3))
;		         )
;			'(3)))

;(check auto7 '(a x) T)
