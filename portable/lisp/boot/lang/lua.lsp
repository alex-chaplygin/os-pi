(defun sixth(x)   (car (cdr (cdr (cdr (cdr (cdr x)))))))
(defun seventh(x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(defun eighth(x)  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(defun ninth(x)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(defun tenth(x)   (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))

(defun is-lua-first-name-char(char)
  "Проверка на допустимый первый символы в идентификаторе Lua"
  (or (is-alpha char) (= char #\_)))

(defun is-lua-name-char(char)
  "Проверка на допустимый первый символы в идентификаторе Lua"
  (or (is-alpha char) (is-digit char) (= char #\_)))

(defun is-lua-sep(char)
  "Предикат проверки на разделитель в Lua"
  (contains (list (code-char 10) #\ ) char))

(defun is-lua-op(char)
  "Предикат проверки на оператор в Lua"
  (contains (list #\- #\+ #\* #\/ #\> #\< #\= #\% #\#) char))

(defun is-lua-comp-op(char)
  "Предикат проверки на оператор сравнения в Lua"
  (contains (list #\= #\> #\< #\~) char))

(defun map-symbol(sym)
  "Преобразует полученный символ в необходимый для исполнения сгенерированных Lisp-выражений"
  (case sym
	('== 'lua-eq)
	('~= 'lua-not-eq)
	('>= 'lua->=)
	('<= 'lua-<=)
	('> 'lua->)
	('< 'lua-<)
	('+ 'lua-add)
	('- 'lua-sub)
	('/ 'lua-div)
	('* 'lua-mul)
	('= 'lua-set)
	('% 'lua-mod)
	('not 'lua-not)
	('and 'lua-and)
	('or 'lua-or)
	('nil 'nil-const)
	('false 'false-const)
	('true 'true-const)
	(otherwise sym)))

(defun char-to-symbol(char)
  "Преобразует символ строки в Lisp-символ"
  (map-symbol (intern (implode (list char)))))

(defun char-list-to-symbol(char-list)
  "Преобразует список символов в Lisp-символ с именем в верхнем регистре"
  (map-symbol (intern (implode (map #'toupper char-list)))))

(defun parse-str-char(quotes)
  "Разбор символа строки (поддерживает экранирование)"
  (parse-or (parse-pred #'(lambda (char) (and (!= char quotes) (!= char #\\) (!= char (code-char 10)))))
	    (parse-app (&&& (parse-elem #\\) (parse-pred #'(lambda (c) t))) #'second)))

(defun parse-str(quotes)
  "Разбор строки"
  (parse-app (&&& (parse-elem quotes)
		  (parse-app (parse-many (parse-str-char quotes)) #'(lambda (char-list) (implode char-list)))
		  (parse-elem quotes))
	     #'second))

(defun parse-numeral()
  "Лексический разбор числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun parse-symbol()
  "Разбор символа"
  (parse-or (parse-elem #\()
	    (parse-elem #\))
	    (parse-elem #\,)
	    (parse-elem #\;)
	    (parse-elem #\{)
	    (parse-elem #\})
	    (parse-elem #\[)
	    (parse-elem #\])
	    (parse-elem #\:)
	    (parse-app (parse-elem #\#) (parse-return 'lua-len))
	    (parse-app (&&& (parse-elem #\.) (parse-elem #\.)) (parse-return 'lua-concat))
	    (parse-elem #\.)
	    (parse-app (&&& (parse-pred #'is-lua-comp-op) (parse-elem #\=)) #'char-list-to-symbol)
	    (parse-app (parse-pred #'is-lua-op) #'char-to-symbol)
	    (parse-app (&&& (parse-pred #'is-lua-first-name-char) (parse-many (parse-pred #'is-lua-name-char)))
		  #'(lambda (parts) (char-list-to-symbol (cons (car parts) (second parts)))))))

(defun lua-token()
  "Парсит один из токенов Lua"
  (parse-or (parse-numeral) (parse-str #\') (parse-str #\") (parse-symbol)))

(defun lua-lexer(str)
  "Лексический анализатор Lua"
  (funcall (parse-sep (lua-token) (parse-some (parse-pred #'is-lua-sep))) (stream-from-str str)))

(defun is-lua-keyword(sym)
  "Является ли символ ключевым словом Lua"
  (contains '(if else elseif then end lua-set false-const true-const nil-const for do break repeat until while local function) sym))

(defun is-lua-fieldsep(sym)
  "Является ли символ унарным оператором Lua"
  (contains '(#\; #\,) sym))

(defun is-lua-unop(sym)
  "Является ли символ унарным оператором Lua"
  (contains '(lua-not - lua-len) sym))

(defun is-lua-binop(sym)
  "Является ли символ бинарным оператором Lua"
  (contains '(lua-eq lua-not-eq lua-> lua->= lua-< lua-<= + - * / lua-or lua-and) sym))

(defun is-lua-name(sym)
  "Является ли символ идентификатором переменной"
  (and (not (is-lua-binop sym)) (not (is-lua-keyword sym)) (not (is-lua-unop sym))))

(defun translatel(exp)
  "Преобразует выражение в левоассоциативное Lisp-выражение"
  (let ((expr (append (list (car exp)) (second exp))))
    (if (null (cdr expr))
      (car expr) 
      (let ((first-elem (car expr))
            (operations (cdr expr)))
        (foldl #'(lambda (acc op)
                 (list (car op) acc (second op)))
               first-elem
               operations)))))

(defun translate-if(stat)
  "Транслирует условный оператор if/elseif/else"
  (let ((conditions (map #'(lambda (x) `((lua-is-true ,(second x)) ,(forth x))) (fifth stat))))
      (if (null (sixth stat))
	  `(cond ((lua-is-true ,(second stat)) ,(forth stat)) ,@conditions)
	  `(cond ((lua-is-true ,(second stat)) ,(forth stat)) ,@conditions (t ,(second (sixth stat)))))))

(defun translate-for(stat)
  "Транслирует цикл for"
  `(let ((,(second stat) ,(forth stat))
	  (_limit ,(sixth stat))
	  (_step ,(if (null (seventh stat)) 1 (second (seventh stat)))))
     (while (lua-is-true
	     (lua-or
	      (lua-and (lua-> _step 0) (lua-<= ,(second stat) _limit))
	      (lua-and (lua-<= _step 0) (lua->= ,(second stat) _limit))))
       ,(ninth stat)
       (setq ,(second stat) (+ ,(second stat) _step)))))

(defun translate-while(stat)
  "Транслирует цикл while"
  `(while (lua-is-true ,(second stat)) ,(forth stat)))

(defun translate-until(stat)
  "Транслирует цикл repeat until"
  `(until (lua-is-true ,(forth stat)) ,(second stat)))

(defun translate-list(l)
  "Транслирует список аргументов для вызова или определения функции"
  `(,(car l) ,@(if (null (second l)) nil (map #'second (second l)))))

(defun translate-function(func)
  "Транслирует определение функции"
  `(function (lambda ,(car (second func)) ,(second (second func)))))

(defun translate-functionset(func)
  "Транслирует определение глобальной функции"
  (if (pairp (second func))
    `(lua-set-index ,(second (second func)) ,(third (second func)) (function (lambda ,(car (third func)) ,(second (third func)))))
    `(setq ,(second func) (function (lambda ,(car (third func)) ,(second (third func)))))))

(defun translate-functionselfset(func)
  "Транслирует определение глобальной функции"
    `(lua-set-index ,(second func) ,(symbol-name (forth func)) (function (lambda (self ,@(car (fifth func))) ,(second (fifth func))))))

(defun translate-localfunction(func)
  "Транслирует определение локальной функции"
  `(let (,(third func) (function (lambda ,(car (forth func)) ,(second (forth func)))))
     ,(fifth func)))

(defun translate-prefixexp(prefixexp)
  "Транслирует вызов функции (немедленный вызов поддерживается)"
  (if (null (second prefixexp))
      (car prefixexp)
    (let ((func-name (car prefixexp))
	  (lists (second prefixexp))) 
	 (foldl #'(lambda (acc lst)
		    (let ((op (car lst))
			  (args (cdr lst)))
		      (if (eq op 'funcall-self)
			  `(let ((__t ,acc)) (funcall (lua-get-index  __t ,(second lst)) __t ,@(third lst)))
			  `(,op ,acc ,@args))))
           func-name
           lists))))

(defun translate-funcname(prefixexp)
  "Транслирует имя функции для присваивания таблице"
  (if (null (second prefixexp))
      (car prefixexp)
    (let ((func-name (car prefixexp))
	  (lists (second prefixexp))) 
	 (foldl #'(lambda (acc lst) `(lua-get-index ,acc ,(symbol-name lst)))
           func-name
           lists))))

(defun parse-const()
  "Парсит константы true, false и nil"
  (parse-pred #'(lambda (sym) (contains '(true-const false-const nil-const) sym))))

(defun parse-name()
  "Парсит имя переменной - символ"
  (parse-pred #'(lambda (sym) (and (symbolp sym) (is-lua-name sym)))))

(defun parse-num()
  "Парсит числовую константу"
  (parse-pred #'integerp))

(defun parse-string()
  "Парсит строковую константу"
  (parse-pred #'stringp))

(defun parse-exp())

(defun parse-namelist()
  "Парсит список имен Name"
  (parse-app (&&& (parse-name) (parse-many (&&& (parse-elem #\,) (parse-name))))
	     #'translate-list))

(defun parse-explist()
  "Парсит список выражений exp через запятую"
  (parse-app (&&& (parse-exp) (parse-many (&&& (parse-elem #\,) (parse-exp))))
	     #'translate-list))

(defun parse-args()
  "Парсит аргументы для определения функции"
  (parse-app (&&& (parse-elem #\() (parse-optional (parse-namelist)) (parse-elem #\))) #'second))

(defun parse-callargs()
  "Парсит аргументы для вызова функции"
  (parse-app (&&& (parse-elem #\() (parse-optional (parse-explist)) (parse-elem #\))) #'second))

(defun parse-block () nil)
(defun parse-primary () nil)
(defun parse-prefixexp-tail () nil)

(defun parse-funcbody()
  "Парсит тело функции с аргументами"
  (&&& (parse-args) (parse-rec (parse-block)) (parse-elem 'end)))

(defun parse-function()
  "Парсит определение функции"
  (parse-app (&&& (parse-elem 'function) (parse-funcbody)) #'translate-function))

(defun parse-funcname()
  (parse-app (&&& (parse-name) (parse-many (parse-app (&&& (parse-elem #\.) (parse-name)) #'second)))
	     #'translate-funcname))

(defun parse-functioncall()
  "Парсит вызов функции"
  (parse-app #'(lambda (stream)
      (let ((res (funcall (&&& (parse-primary) (parse-some (parse-prefixexp-tail))) stream)))
	(if (null res) nil
	  (let ((op (car (last (second (car res)))))) (if (or (eq op 'funcall) (eq op 'funcall-self)) res nil)))))
	     #'translate-prefixexp))

(defun parse-var()
  "Парсит "
  (parse-app #'(lambda (stream)
      (let ((res (funcall (&&& (parse-primary) (parse-many (parse-prefixexp-tail))) stream)))
	(if (null res) nil
	  (if (not (pairp (second res))) res
	    (if (not (pairp (car (second res)))) res
	    (if (= (car (last (second (car res)))) 'lua-get-index) res nil))))))
	     #'translate-prefixexp))

(defun parse-varlist()
  "Парсит список имен Name"
  (parse-app (&&& (parse-var) (parse-many (&&& (parse-elem #\,) (parse-var))))
	     #'translate-list))

(defun parse-field()
  (parse-or
   (parse-app (&&& (parse-elem #\[) (parse-exp) (parse-elem #\]) (parse-elem 'lua-set) (parse-exp))
	      #'(lambda (field) `(cons ,(second field) ,(fifth field))))
   (parse-app (&&& (parse-name) (parse-elem 'lua-set) (parse-exp))
	      #'(lambda (field) `(cons ,(symbol-name (car field)) ,(third field))))
   (parse-app (parse-exp)
	      #'(lambda (field) `(cons 'indexed ,field)))))

(defun parse-fieldlist()
  (parse-app (&&& (parse-field) (parse-many (&&& (parse-pred #'is-lua-fieldsep) (parse-field))) (parse-optional (parse-pred #'is-lua-fieldsep)))
	     #'(lambda (field) `(list ,(car field) ,@(map #'second (second field))))))

(defun parse-tableconstructor()
  (parse-app (&&& (parse-elem #\{) (parse-optional (parse-fieldlist)) (parse-elem #\}))
	     #'(lambda (table) `(lua-createtable ,(second table)))))

(defun parse-primary()
  "Парсит первичную составляющую выражения - имя или другое выражение в скобочках"
  (parse-or
   (parse-name)
   (parse-app (&&& (parse-elem #\( ) (parse-rec (parse-exp)) (parse-elem #\))) #'second)))

(defun parse-prefixexp()
  "Парсит выражение со следующими справа хвостами вызова функции, индексации или обращения к полю"
  (parse-app (&&& (parse-primary) (parse-many (parse-prefixexp-tail))) #'translate-prefixexp))

(defun parse-prefixexp-tail()
  "Парсит индексацию, обращение к полю или вызов функции"
  (parse-or
   (parse-app (&&& (parse-elem #\[) (parse-rec (parse-exp)) (parse-elem #\]))
	      #'(lambda (name) `(lua-get-index ,(second name))))
   (parse-app (&&& (parse-elem #\.) (parse-name))
	      #'(lambda (name) `(lua-get-index ,(symbol-name (second name))))) 
   (parse-app (&&& (parse-optional (&&& (parse-elem #\:) (parse-name))) (parse-callargs))
	      #'(lambda (callargs) (if (null (car callargs))
				       (cons 'funcall (second callargs))
				       `(funcall-self ,(symbol-name (second (car callargs))) ,(second callargs)))))))


(defun parse-value()
  "Парсит выражение (константа, число, опреление функции, другое выражение)"
   (parse-or
    (parse-num)
    (parse-string)
    (parse-tableconstructor)
    (parse-function)
    (parse-prefixexp)
    (parse-const)))

(defun parse-factor()
  "Парсит множитель"
  (parse-app (&&& (parse-optional (parse-pred #'is-lua-unop)) (parse-value))
	     #'(lambda (factor) (if (null (car factor)) (second factor) factor))))

(defun parse-term()
  "Парсит слагаемое"
  (parse-app (&&& (parse-factor) (parse-many (parse-or
	 (&&& (parse-elem 'lua-mul) (parse-factor))
	 (&&& (parse-elem 'lua-div) (parse-factor))
	 (&&& (parse-elem 'lua-mod) (parse-factor)))))
	     #'translatel))

(defun parse-concater()
  "Парсит элемент конкатенации"
  (parse-app (&&& (parse-term) (parse-many (parse-or
	 (&&& (parse-elem 'lua-add) (parse-term))
	 (&&& (parse-elem 'lua-sub) (parse-term)))))
	     #'translatel))

(defun parse-comparand()
  "Парсит элемент сравнения"
  (parse-app (&&& (parse-concater) (parse-many (&&& (parse-elem 'lua-concat) (parse-concater)))) #'translatel))

(defun parse-conjunct()
  "Парсит элемент конъюкции"
  (parse-app (&&& (parse-comparand) (parse-many (parse-or
	 (&&& (parse-elem 'lua-eq) (parse-comparand))
	 (&&& (parse-elem 'lua-not-eq) (parse-comparand))
	 (&&& (parse-elem 'lua->) (parse-comparand))
	 (&&& (parse-elem 'lua-<) (parse-comparand))
	 (&&& (parse-elem 'lua->=) (parse-comparand))
	 (&&& (parse-elem 'lua-<=) (parse-comparand)))))
	     #'translatel))

(defun parse-disjunct()
  "Парсит элемент дизъюнкции"
  (parse-app (&&& (parse-conjunct) (parse-many 
	 (&&& (parse-elem 'lua-and) (parse-conjunct))))
	     #'translatel))

(defun parse-exp()
  "Парсит выражение"
  (parse-app (&&& (parse-disjunct) (parse-many 
	 (&&& (parse-elem 'lua-or) (parse-disjunct))))
	     #'translatel))

(defun parse-stat()
  "Парсит инструкцию"
  (parse-or
   ;; stat = varlist "=" explist
   (parse-app (&&& (parse-varlist) (parse-elem 'lua-set) (parse-explist))
	      #'(lambda (stat) `(lua-set ,(car stat) ,(third stat))))
   ;; stat = "local" Name "=" exp
   (parse-app (&&& (parse-elem 'local) (parse-name) (parse-elem 'lua-set) (parse-exp) (parse-rec (parse-block)))
	      #'(lambda (stat) `(let ((,(second stat) ,(forth stat))) ,(fifth stat))))
   ;; stat = "if" exp "then" block {"elseif" exp "then" block} ["else" block] "end"
   (parse-app (&&& (parse-elem 'if) (parse-exp) (parse-elem 'then) (parse-rec (parse-block))
		   (parse-many (&&& (parse-elem 'elseif) (parse-exp) (parse-elem 'then) (parse-rec (parse-block))))
		   (parse-optional (&&& (parse-elem 'else) (parse-rec (parse-block))))
		   (parse-elem 'end))
	      #'translate-if)
   ;; stat = "for" Name "=" exp "," exp ["," exp] "do" block "end"
   (parse-app (&&& (parse-elem 'for) (parse-name) (parse-elem 'lua-set) (parse-exp)
		   (parse-elem #\,) (parse-exp) (parse-optional (&&& (parse-elem #\,) (parse-exp)))
		   (parse-elem 'do) (parse-rec (parse-block)) (parse-elem 'end))
	      #'translate-for)
   ;; stat = "repeat" block "until" exp
   (parse-app (&&& (parse-elem 'repeat) (parse-rec (parse-block)) (parse-elem 'until) (parse-exp))
	      #'translate-until)
   ;; stat = "while" exp "do" block "end"
   (parse-app (&&& (parse-elem 'while) (parse-exp) (parse-elem 'do) (parse-rec (parse-block)) (parse-elem 'end))
	      #'translate-while)
   ;; stat = "function" funcname funcbody
   (parse-app (&&& (parse-elem 'function) (parse-funcname) (parse-funcbody))
    	      #'translate-functionset)
   (parse-app (&&& (parse-elem 'function) (parse-funcname) (parse-elem #\:) (parse-name) (parse-funcbody))
    	      #'translate-functionselfset)
   ;; stat = "local" "function" Name funcbody
   (parse-app (&&& (parse-elem 'local) (parse-elem 'function) (parse-name) (parse-funcbody) (parse-rec (parse-block)))
	      #'translate-localfunction)
   ;; stat = functioncall
   (parse-functioncall)
   ))

(defun parse-laststat()
  "Парсит последнюю инструкцию в блоке: return exp или break"
  (parse-or (parse-app (parse-elem 'break) (parse-return `(go break)))
	    (parse-app (&&& (parse-elem 'return) (parse-exp)) #'second)))

(defun parse-block()
  "Парсит блок инструкций"
  ;; block = {stat [";"]} [laststat [";"]]
  (parse-app (&&& (parse-many (&&& (parse-stat) (parse-optional (parse-elem #\;))))
		  (parse-optional (&&& (parse-laststat) (parse-optional (parse-elem #\;)))))
	     #'(lambda (block)
		 (if (null (second block))
		     `(progn ,@(map #'(lambda (stat) (car stat)) (car block)))
		     `(progn ,@(map #'(lambda (stat) (car stat)) (car block)) ,(car (second block)))))))

(defun lua-to-lisp(str)
  "Преобразует строку с кодом на Lua в Lisp выражение"
  (car (funcall (parse-block) (stream-from-list (car (lua-lexer str))))))
