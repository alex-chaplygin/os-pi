;; Библиотека регулярных выражений

;; Грамматики регулярных выражений
;; Выражение = ['^'] Последовательность { '|' Последовательность } ['$']
;; Последовательность = Элемент {Элемент}
;; Элемент = (cимвол | Группа | Диапазон | СимвольныйКласс | '.') [Квантификатор]
;; Квантификатор = ('*' | '?' | '+' | Количество) ['?']
;; Количество = '{' число [',' число | ','] '}'
;; Диапазон =  '[' [ '^' ] (символ [ '-' символ ] | СимвольныйКласс) { (символ [ '-' символ ] | СимвольныйКласс)  } ']'
;; СимвольныйКласс = '\' ( 'w' | 'W' | 'd' | 'D' | 's' | 'S' )
;; Группа = '(' [ '?' ':' ]  Выражение ')'

;; Предикат символа выражения, исключаются спец. сиволы
(defun regex-sym (x) (not (contains '(#\( #\) #\* #\| #\. #\? #\+ #\{ #\} #\\ #\[ #\]) x)))

;; Предикат класса символов
(defun sym-class (x) (contains '(#\w #\W #\d #\D #\s #\S) x))

;; Разбор символа      
(defun parse-sym ()
  (parse-or
   (parse-app (&&& (parse-elem #\\) (parse-pred #'(lambda (x) (not (regex-sym x))))) #'(lambda (x) (list 'sym (second x)))) ;; экранирование символов
   (parse-app (parse-pred #'regex-sym) #'(lambda (x) (list 'sym x)))))

;; '.'
(defun parse-any ()
  (parse-app (parse-elem #\.) (parse-return '(any))))

;; Предикат специальных символов в диапазоне
(defun range-special-sym (x) (contains '(#\[ #\] #\\ #\^) x))

;; Разбор символа в диапазоне
(defun parse-range-sym ()
  (parse-app (parse-or (parse-app (&&& (parse-elem #\\) (parse-pred #'range-special-sym)) #'(lambda (x) (second x))) ;; экранирование символов
		       (parse-pred #'(lambda (x) (not (range-special-sym x)))))
	     #'(lambda (x) (char-code x))))

;; СимвольныйКласс = '\' ( 'w' | 'W' | 'd' | 'D' | 's' | 'S' )
(defun parse-sym-class ()
  (parse-app (&&& (parse-elem #\\) (parse-pred #'sym-class))
	     #'(lambda (x) `(class ,(case (second x)
				      (#\w 'word)
				      (#\W 'nword)
				      (#\d 'digit)
				      (#\D 'ndigit)
				      (#\s 'space)
				      (#\S 'nspace))))))

;; Диапазон =  '[' [ '^' ] (символ [ '-' символ ] | СимвольныйКласс) { (символ [ '-' символ ] | СимвольныйКласс)  } ']'
(defun parse-range ()
  (parse-app (&&& (parse-elem #\[)
		  (parse-optional (parse-app (parse-elem #\^) (parse-return 'negative)))
		  (parse-some (parse-or (&&& (parse-range-sym)
					     (parse-optional (parse-app (&&& (parse-elem #\-) (parse-range-sym)) #'(lambda (x) (second x)))))
					(parse-sym-class)))
		  (parse-elem #\]))
	     #'(lambda (x) (list 'range (second x) (third x)))))

(defun parse-expression () nil)

;; Группа = '(' Выражение ')'
(defun parse-group ()
  (parse-app (&&& (parse-elem #\()
		  (parse-optional (&&& (parse-elem #\?) (parse-elem #\:)))
		  (parse-expression)
		  (parse-elem #\)))
	     #'(lambda (x) (if (second x) (third x) (list 'group (third x))))))

;; Количество = '{' число [',' число | ','] '}'
(defun parse-count ()
  (parse-app
   (&&& (parse-elem #\{) (parse-decimal)
	(parse-optional (parse-or (parse-app (&&& (parse-elem #\,) (parse-decimal)) #'(lambda (x) (second x)) )
				  (parse-app (parse-elem #\,) (parse-return 'more))))
	(parse-elem #\}))
   #'(lambda (x) (list 'repeat (second x) (when (= (list-length x) 4) (third x))))))

;; Квантификатор = ('*' | '?' | '+' | Количество) ['?']
(defun parse-quant ()
  (parse-app
   (&&&
    (parse-or (parse-app (parse-elem #\*) (parse-return '(star)))
	      (parse-app (parse-elem #\?) (parse-return '(question)))
	      (parse-app (parse-elem #\+) (parse-return '(plus)))
	      (parse-count))
    (parse-optional (parse-elem #\?)))
   #'(lambda (x) (list (car x) (when (second x) 'lazy)))))

(defun app-quant (x)
  "Применяет квантификатор к элементу"
  (if (second x)
      (let ((quant (car (second x)))
	    (lazy (second (second x))))
	(case (car quant)
	  ('repeat
	   (let* ((start-count (second quant))
		  (end-count (third quant))) ; может быть числом, символом MORE или пустым списком
	     (cond
	       ((and (<= start-count 0) (null end-count)) '(epsilon))
	       ((and (<= start-count 0) (= end-count 'more)) `(,(if lazy 'lazy-star 'star) ,(car x)))
	       ((and (<= start-count 0) (= end-count 1)) (if lazy `(or (epsilon) ,(car x)) `(or ,(car x) (epsilon))))
	       ((and (= start-count 1) (null end-count)) (car x))
	       (t `(seq
		    ,@(make-list start-count (car x))
		    ,@(cond
			((= end-count 'more) `((,(if lazy 'lazy-star 'star) ,(car x))))
			((integerp end-count) (make-list end-count (if lazy `(or (epsilon) ,(car x)) `(or ,(car x) (epsilon)))))
			(t '())))))))
	  
	  ('star (app-quant (list (car x) `((repeat 0 more) ,lazy))))
	  ('question (app-quant (list (car x) `((repeat 0 1) ,lazy))))
	  ('plus (app-quant (list (car x) `((repeat 1 more) ,lazy))))
	  (otherwise (car x))))
      (car x)))

;; Элемент = (cимвол | Группа | Диапазон | СимвольныйКласс | '.') [Квантификатор]
(defun parse-element ()
  (parse-app (&&& (parse-or (parse-sym)
			    (parse-rec (parse-group))
			    (parse-range)
			    (parse-app (parse-sym-class) #'(lambda (x) `(range () (,x))))
			    (parse-any))
		  (parse-optional (parse-quant)))
	     #'app-quant))

;; Последовательность = Элемент {Элемент}
(defun parse-seq ()
  (parse-app (parse-some (parse-element)) #'(lambda (x) (if (= (list-length x) 1) (car x) (cons 'seq x)))))

;; Выражение = Последовательность { '|' Последовательность }
(defun parse-expression ()
  (parse-app (&&& (parse-seq)
		  (parse-many (parse-app (&&& (parse-elem #\|) (parse-seq)) #'second )))
	     #'(lambda (x) (if (second x) (cons 'or (cons (car x) (second x))) (car x)))))

;; оборачиваем в (group) чтобы захватывать все выражение
(defun parse-regex ()
  (parse-app (parse-expression) #'(lambda (x) (list 'group x))))

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
     ,(third a2)
     ,(cadddr a2)
     ,(caddddr a2)))

(defun or-nfa(a1 a2)
"Добавляет к автомату a1 новую альтернативу a2"
  `(,(car a1)
    (,@(second a1)
     (,(car a1) E (,(car a2)))
     ,@(second a2)
     (,(third a2) E (,(third a1))))
     ,(third a1)
     ,(cadddr a2)
     ,(caddddr a2)))

(defun regex-to-nfa (regex) nil)

(defun list-nfa(regex nfa f)
"Рекурсивно создает автомат из последовательности, комбинируя функцией f"
  (let* ((a (regex-to-nfa (car regex) (cadddr nfa) (caddddr nfa)))
	 (new-nfa (funcall f nfa a)))
    (if (cdr regex)
	(list-nfa (cdr regex) new-nfa f)
	new-nfa)))


;; Возвращает предикаты для символьных классов \w \W \d \D \s
(defun predefined-preds (class)
  (let ((word '(or (and (>= sym 48) (<= sym 57)) (and (>= sym 65) (<= sym 90)) (and (>= sym 97) (<= sym 122)) (= sym 95)))
	(digit '(and (>= sym 48) (<= sym 57)))
	(space '(or (= sym 9) (= sym 10) (= sym 12) (= sym 13) (= sym 32))))
    (case class
      ('word word)
      ('nword `(not ,word))
      ('digit digit)
      ('ndigit `(not ,digit))
      ('space space)
      ('nspace `(not ,space)))))

(defun gen-preds (negative ranges)
  "Возвращает предикаты для диапазона символов"
  (labels ((s1 (ranges acc)
	     (if (null ranges) acc
		 (s1 (cdr ranges)
			    (let* ((range (car ranges))
				   (start (car range))
				   (end (second range)))
			      (cons
			       (if (= start 'class)
				   (predefined-preds end)
				   (if end `(and (>= sym ,start) (<= sym ,end)) `(= sym ,start)))
			       acc))))))
    (let ((result `(or ,@(s1 ranges nil))))
      #'(lambda (x) (eval `(let ((sym ,x))
             ,(if negative `(not ,result) result)))))))

(defun regex-to-nfa (regex groups preds)
  "Преобразует разобранное регулярное выражение в НКА"
  (let ((start (new-state))
	(end (new-state)))
    (case (car regex)
      ('any `(,start ((,start any (,end))) ,end ,groups ,preds))
      ('sym `(,start ((,start ,(second regex) (,end))) ,end ,groups ,preds))
      ('star (let ((r (regex-to-nfa (second regex) groups preds)))
	       `(,start  ,(append (second r)
				  `((,start E (,(car r)))
				    (,(third r) E (,(car r)))
				    (,(third r) E (,end))
				    (,start E (,end))))
			       ,end ,(cadddr r) ,(caddddr r))))
      ('lazy-star (let ((r (regex-to-nfa (second regex) groups preds)))
		    `(,start ,(append (second r)
				      `((,start E (,end))
					(,(third r) E (,end))
					(,start E (,(car r)))
					(,(third r) E (,(car r)))))
				   ,end ,(cadddr r) ,(caddddr r))))
      ('epsilon `(,start ((,start E (,end))) ,end ,groups ,preds))
      ('seq (list-nfa (cddr regex) (regex-to-nfa (second regex) groups preds) #'seq-nfa))
      ('or (list-nfa (cdr regex) `(,start () ,end ,groups ,preds) #'or-nfa))
      ('group (let ((r (regex-to-nfa (second regex) groups preds)))
		`(,start ,(append (second r)
				  `((,start E (,(car r)))
				    (,(third r) E (,end))))
			 ,end
			 ,(append (cadddr r) `((,(cons start 'E) ,(cons (third r) 'E))))
			 ,(caddddr r))))
      ('range (let ((negative (second regex))
		    (ranges (third regex)))
		`(,start ((,start pred (,end))) ,end ,groups ,(cons `(,(cons start 'pred) ,(gen-preds negative ranges)) preds))))
      )))

(defun compile-regex (str)
  "Компилирует регулярное выражение в недетерминированный автомат"
  (regex-to-nfa (car (funcall (parse-regex) (stream-from-str str))) nil nil))

(defun backtrack-regex(nfa tape until-end)
  "Проходится по строке tape и возвращает список совпадений. Если совпадение не найдено, возвращается nil (пустой список)"
  "Первый элемент в списке совпадений - все выражение, последующие - группы захвата"
  (let* ((rules (cadr nfa))
	 (final-states (caddr nfa))
	 (str-length (list-length tape))
	 (groups (cadddr nfa))
	 (preds (caddddr nfa))
	 (groups-result (make-hash)))
    (labels ((process-states (states index)
	       (if (null states) nil
		   (let* ((state (car states))
			  (epsilon-state (cons state *epsilon*))
			  (any-char-state (cons state *any-char*))
			  (pred-state (cons state *pred*))
			  (next-index (+ index 1))
			  (next-state (cond
					((check-key rules epsilon-state)
					 (progn
					   (when (check-key groups epsilon-state)
					     (let* ((group (get-hash groups epsilon-state))
						    (group-index (second group))
						    (prev (if (check-key groups-result group-index) (get-hash groups-result group-index) '(() ()))))
					       (set-hash groups-result group-index
							 (if (equal (car group) 'start)
							     (list index (second prev))
							     (list (car prev) index)))))
					   (process-states (get-hash rules epsilon-state) index)))
					
					((and (< index str-length)
					      (check-key rules (cons state (nth tape index))))
					 (process-states (get-hash rules (cons state (nth tape index))) next-index))

					((and (< index str-length)
					      (check-key rules pred-state)
					      (check-key preds pred-state)
					      (let ((func (get-hash preds pred-state)))
						(funcall func (char-code (nth tape index)))))
					 (process-states (get-hash rules pred-state) next-index))
					
					((and (< index str-length)
					      (check-key rules any-char-state))
					 (process-states (get-hash rules any-char-state) next-index))
					
					(t (if (and (if until-end (= index str-length) t) (nfa-end (list `(,state) rules final-states)))
					       state
					       (process-states (cdr states) index)) ))))
		     (if next-state next-state
			 (process-states (cdr states) index)))))

	     (process-groups (groups acc)
	       (if (null groups) acc
		   (let* ((result ())
			  (group (car groups))
			  (start (second group))
			  (end (third group)))
		    
		     (when (and start end)
			 (for i start end
			      (setq result (append result `(,(nth tape i))))))
		     (process-groups (cdr groups) (append acc `(,(implode result))))))))
      
      (when (process-states (car nfa) 0)
	 (process-groups (cdr groups-result) ())))))

(defun match (str regex)
  "Поиск первого соответствия строки str строке регулярного выражения regex"
  (let* ((exploded-regex (explode regex))
	 (start (= (car exploded-regex) #\^))
	 (end (= (last exploded-regex) #\$)))
    (when start (setq exploded-regex (cdr exploded-regex))) 
    (when end (setq exploded-regex (reverse (cdr (reverse exploded-regex)))))
    (let* ((compiled (compile-regex (implode exploded-regex)))
	   (nfa (make-backtrack-nfa (car compiled) (second compiled) (list (third compiled)) (cadddr compiled) (caddddr compiled)))
	   (exploded-str (explode str)))
      (if start
	  (backtrack-regex nfa exploded-str end)
	  (labels ((s1 (tape)
		     (let ((result (backtrack-regex nfa tape end)))
		       (cond
     			 (result result)
			 ((null tape) nil)
     			 (t (s1 (cdr tape)))))))
	    (s1 exploded-str))))))

(defun matches (str regex)
  "Поиск всех соответствий строки str строке регулярного выражения regex"
  (let* ((exploded-regex (explode regex))
	 (start (= (car exploded-regex) #\^))
	 (end (= (last exploded-regex) #\$)))
    (when start (setq exploded-regex (cdr exploded-regex))) 
    (when end (setq exploded-regex (reverse (cdr (reverse exploded-regex)))))
    (let* ((compiled (compile-regex (implode exploded-regex)))
	   (nfa (make-backtrack-nfa (car compiled) (second compiled) (list (third compiled)) (cadddr compiled) (caddddr compiled)))
	   (exploded-str (explode str)))
      (if start
	  (let ((result (backtrack-regex nfa exploded-str end)))
	    (if result `(,result) ()))
	  (labels ((s1 (tape acc)
		     (let ((result (backtrack-regex nfa tape end)))
		       (if (null tape) acc
     			   (s1 (cdr tape) (if result (append acc `(,result)) acc))))))
	    (s1 exploded-str nil))))))
