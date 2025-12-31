;; ; Библиотека регулярных выражений

;; Грамматики регулярных выражений
;; Выражение = Последовательность { '|' Последовательность }
;; Последовательность = Элемент {Элемент}
;; Элемент = (cимвол | Группа) {'*' | '+' | '?'}
;; Группа = '(' Выражение ')'
;; Диапазон = '[' {символ} ']'

;; Предикат символа выражения, исключаются спец. символы
(defun regex-symp (x) (not (contains '(#\( #\) #\* #\+ #\| #\. #\? #\[ #\]) x)))

;; Разбор символа      
(defun regex-sym ()
  (parse-or
   (parse-app (parse-pred #'regex-symp) #'(lambda (x) (list 'sym x)))
   (parse-app (parse-elem #\.) (parse-return '(any)))))

(defun regex-expression () nil)

;; Группа = '(' Выражение ')'
(defun regex-group ()
  (parse-app (&&& (parse-elem #\() (regex-expression) (parse-elem #\))) #'second))

;; Вспомогательная функция для преобразования списка символов
(defun chars-to-syms (chars)
  "Преобразует список символов в список узлов (sym символ)"
  (if (null chars) nil
      (cons (list 'sym (car chars)) (chars-to-syms (cdr chars)))))

;; Диапазон = '[' {символ} ']'
(defun regex-range ()
  (&&& (parse-elem #\[)
       chars-> (parse-some (parse-pred #'(lambda (x) (not (= x #\])))))
       (parse-elem #\])
       return (let ((syms (chars-to-syms chars)))
		(if (= (list-length syms) 1) (car syms)
		    (cons 'or syms)))))

;; Элемент = (cимвол | Группа | Диапазон) {'*' | '+' | '?'} 
(defun regex-element ()
  (&&& elem->(parse-or (regex-sym) 
		       (parse-rec (regex-group))
		       (parse-rec (regex-range)))
       quant->(parse-optional (parse-or (parse-elem #\*) 
					(parse-elem #\+) 
					(parse-elem #\?)))
       return (case quant
		(#\* (list 'star elem))
		(#\+ (list 'seq elem (list 'star elem)))
		(#\? (list 'or (list 'epsilon) elem))
		(otherwise elem))))

;; Последовательность = Элемент {Элемент}
(defun regex-seq ()
  (parse-app (parse-some (regex-element)) #'(lambda (x) (if (= (list-length x) 1) (car x) (cons 'seq x)))))

;; Выражение = Последовательность { '|' Последовательность }
(defun regex-expression ()
  (parse-app (&&& (regex-seq)
		  (parse-many (parse-app (&&& (parse-elem #\|) (regex-seq)) #'second )))
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
      ('epsilon `(,start ((,start E (,end))) ,end))
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
      ('or (list-nfa (cdr regex) `(,start () ,end) #'or-nfa)))))

(defun compile-regex (str)
  "Компилирует регулярное выражение в недетерминированный автомат"
  (let ((nfa (regex-to-nfa (car (funcall (regex-expression) (stream-from-str str))))))
    (build-nfa (car nfa) (second nfa) (list (third nfa)))))

(defun regex-match-full (str regex)
  "Сопоставление строки str  и шаблона регулярного выражения regex для целой строки"
  (nfa-end (foldl #'nfa-input (compile-regex regex) (explode str))))

(defun regex-match (str regex)
  "Сопоставление строки str и шаблона регулярного выражения regex как подстроки"
  (regex-match-full str (concat ".*" regex)))
