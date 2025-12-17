
(defvar true-const 'true)
(defvar false-const 'false)
(defvar nil-const 'lua-nil)

(defvar print #'print)

(defun lua-to-str(a)
  (cond ((stringp a) a)
	((integerp a) (inttostr a))
	((symbolp a) (symbol-name a))
	(t (make-string 0 #\0))))

(defun lua-to-int(a)
  (cond ((stringp a) (strtoint a))
	((integerp a) a)
	(t (error "invalid arg: lua-to-int"))))

(defun lua-is-false(val)
  (or (eq val 'false) (eq val 'lua-nil)))

(defun lua-is-true(val)
  (not (lua-is-false val)))

(defun lua-eq(a b)
  (if (= a b) 'true 'false))

(defun lua-not-eq(a b)
  (if (= a b) 'false 'true))

(defun lua-<=(a b)
  (if (<= a b) 'true 'false))
  
(defun lua->=(a b)
  (if (>= a b) 'true 'false))
  
(defun lua-<(a b)
  (if (< a b) 'true 'false))

(defun lua->(a b)
  (if (> a b) 'true 'false))

(defun lua-add(a b)
  (+ (lua-to-int a) (lua-to-int b)))

(defun lua-sub(a b)
  (- (lua-to-int a) (lua-to-int b)))

(defun lua-div(a b)
  (/ (lua-to-int a) (lua-to-int b)))

(defun lua-mul(a b)
  (* (lua-to-int a) (lua-to-int b)))

(defun lua-mod(a b)
  (mod (lua-to-int a) (lua-to-int b)))

(defun lua-not(a)
  (lua-is-false a))

(defun lua-and(a b)
  (if (lua-is-false a) a b))

(defun lua-or(a b)
  (if (lua-is-true a) a b))

(defun lua-concat(a b)
  (concat (lua-to-str a) (lua-to-str b)))

(defmacro lua-while (test &rest bod)
  "Цикл while"
  (let ((loops (gensym))
	(tests (gensym)))
    `(tagbody
	(go ,tests)
	,loops
	,@bod
	,tests
	(if ,test (go ,loops) nil)
	lua-break
	)))

(defmacro lua-until (test &rest bod)
  "Цикл while"
  (let ((loops (gensym))
	(tests (gensym)))
    `(tagbody
	(go ,loops)
	,loops
	,@bod
	,tests
	(if ,test (go ,loops) nil)
	lua-break
	)))
