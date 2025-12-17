;; ; Библиотека регулярных выражений

;; Грамматики регулярных выражений
;; Выражение = Последовательность { '|' Последовательность }
;; Последовательность = Элемент {Элемент}
;; Элемент = (cимвол | Группа) {'*'}
;; Группа = '(' Выражение ')'

;; Предикат символа выражения, исключаются спец. сиволы
(defun regex-sym (x) (not (contains '(#\( #\) #\* #\| #\.) x)))
;; Разбор символа      
(defun parse-sym ()
  (parse-or
   (parse-app (parse-pred #'regex-sym) #'(lambda (x) (list 'sym x)))
   (parse-app (parse-elem #\.) (parse-return '(any)))))

(defun parse-expression () nil)

;; Группа = '(' Выражение ')'
(defun parse-group ()
  (parse-app (&&& (parse-elem #\() (parse-expression) (parse-elem #\))) #'second))

;; Элемент = (cимвол | Группа) {'*' | '+'}
(defun parse-element ()
  (parse-app (&&& (parse-or (parse-sym) (parse-rec (parse-group)))
                  (parse-optional (parse-or (parse-elem #\*) (parse-elem #\+))))
	     #'(lambda (x) (case (second x)
			     (#\* (list 'star (car x)))
			     (#\+ (list 'seq (car x) (list 'star (car x))))
			     (otherwise (car x))))))

;; Последовательность = Элемент {Элемент}
(defun parse-seq ()
  (parse-app (parse-some (parse-element)) #'(lambda (x) (if (= (list-length x) 1) (car x) (cons 'seq x)))))

;; Выражение = Последовательность { '|' Последовательность }
(defun parse-expression ()
  (parse-app (&&& (parse-seq)
		  (parse-many (parse-app (&&& (parse-elem #\|) (parse-seq)) #'second )))
	     #'(lambda (x) (if (second x) (cons 'or (cons (car x) (second x))) (car x)))))

;; Преобразование результата разбора в автомат
;; Текущий счетчик состояния
(defvar *state* 0)

(defun reset-state()
  "Возврат в начальное состояние"
  (setq *state* 0))

(defun new-state()
  "Возвращает очередное состояние"
  (setq *state* (++ *state*)))

(defun seq-nfa(a1 a2)
"Создает новый автомат из двух существующих последовательных автоматов a1 и a2"
  `(,(car a1)
    (,@(second a1)
     (,(third a1) E (,(car a2)))
     ,@(second a2))
    ,(third a2)))

(defun or-nfa(a1 a2)
"Добавляет к автомату a1 новую альтернативу a2"
  `(,(car a1)
    (,@(second a1)
     (,(car a1) E (,(car a2)))
     ,@(second a2)
     (,(third a2) E (,(third a1))))
    ,(third a1)))

(defun regex-to-nfa (regex) nil)

(defun list-nfa(regex nfa f)
"Рекурсивно создает автомат из последовательности, комбинируя функцией f"
  (let* ((a (regex-to-nfa (car regex)))
	 (new-nfa (funcall f nfa a)))
    (if (cdr regex)
	(list-nfa (cdr regex) new-nfa f)
	new-nfa)))

(defun regex-to-nfa (regex)
"Преобразует разобранное регулярное выражение в НКА"
  (let ((start (new-state))
	(end (new-state)))
    (case (car regex)
	  ('any `(,start ((,start any (,end))) ,end))
	  ('sym `(,start ((,start ,(second regex) (,end))) ,end))
	  ('star (let* ((r (regex-to-nfa (second regex))))
		   `(,start  ,(append (second r)
				 `((,start E (,end))
				  (,start E (,(car r)))
				  (,(third r) E (,end))
				  (,(third r) E (,(car r)))))
			 ,end)))
	 ('seq (list-nfa (cddr regex) (regex-to-nfa (second regex)) #'seq-nfa))
	 ('or (list-nfa (cdr regex) `(,start () ,end) #'or-nfa))
	 )))

(defun compile-regex (str)
  "Компилирует регулярное выражение в недетерминированный автомат"
  (regex-to-nfa (car (funcall (parse-expression) (stream-from-str str)))))

(defun regex-match (regex str)
  "Сопоставление скомпилированного регулярного выражения regex и строки str"
  (let* ((nfa (make-nfa (car regex) (second regex) (list (third regex)))))
    (labels ((s1 (tape)
	       (cond ((null tape) nil)
		     ((nfa-end (foldl #'nfa-input nfa tape)) t)
		     (t (s1 (cdr tape))))))
      (s1 (explode str)))))

(defun match (str regex)
  "Сопоставление строки str и строки регулярного выражения regex"
  (regex-match (compile-regex regex) str))
