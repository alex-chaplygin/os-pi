
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

(defun lua-len(x)
  (cond ((stringp x) (string-size x))
	((pairp x) (- (list-length x) 1))))

(defun get-hash-unchecked* (hash key)
  "Возвращает значение по ключу key из таблицы hash"
  (if (null hash) nil-const
  (if (equal key (caar hash)) (cdar hash)
    (get-hash-unchecked* (cdr hash) key))))

(defun get-hash-unchecked (hash key)
  (if (empty-hash hash) nil-const
  (get-hash-unchecked* (cdr hash) key)))

(defun remove-key-unchecked* (prev hash key)
  (unless (null hash) 
  (if (equal key (caar hash)) (rplacd prev (cdr hash))
    (remove-key-unchecked* (cdr prev) (cdr hash) key))))

(defun remove-key-unchecked (hash key)
  "Удаляет ключ key в таблице hash, без проверки вхождения"
  (unless (null hash)
    (remove-key-unchecked* hash (cdr hash) key)))

(defun lua-set-index(table key value)
  (when (eq key 'lua-nil) (error "table index is nil"))
  (if (eq value 'lua-nil)
      (remove-key-unchecked table key)
      (set-hash table key value)))

(defun lua-get-index(table key)
  (when (eq key 'lua-nil) (error "table index is nil"))
  (get-hash-unchecked table key))

(defun lua-createtable(fieldlist)
  (let ((table (make-hash))
	(index 0))
    (map
     #'(lambda (field)
	 (if (eq (car field) 'indexed)
	     (set-hash table (setq index (++ index)) (cdr field))
	     (set-hash table (car field) (cdr field))))
     fieldlist)
    table))

(defmacro lua-set-internal(varlist exps)
  `(progn ,@(let ((i -1)) (map #'(lambda (var)
			   (let ((exp (if (<= (incf i) (list-length exps)) (nth exps i) 'nil-const)))
			       (if (pairp var)
				 `(lua-set-index ,(second var) ,(third var) ,exp)
			         `(setq ,var ,exp))))
			       varlist))))

(defmacro lua-set(varlist explist)
  (let ((exps (map #'(lambda (exp) (list (gensym) exp)) explist)))
    
   `(let ,exps (lua-set-internal ,varlist ,(map #'car exps)))))


