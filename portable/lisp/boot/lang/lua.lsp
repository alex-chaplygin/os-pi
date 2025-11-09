
(defun lua-separator(sym)
  "Предикат проверки на разделитель в Lua"
  (contains (list (code-char 10) #\ ) sym))

(defun lua-operator(sym)
  (contains (list #\- #\+ #\* #\/ #\= #\( #\) ) sym))

(defun parse-symbol()
  "Разбор идентификатора"
  (parse-app (parse-some (parse-pred #'is-alpha))
	     #'(lambda (char-list) (intern (implode (map #'toupper char-list))))))

(defun parse-numeral()
  "Лексический разбор числа"
  (parse-or (parse-hex) (parse-decimal)))

(defun parse-operator()
  "Разбор оператора"
  (parse-pred #'lua-operator))

(defun lua-token()
  "Парсит один из токенов Lua"
  (parse-or (parse-symbol) (parse-numeral) (parse-operator)))

(defun lua-lexer(str)
  "Лексический анализатор Lua"
  (funcall (parse-sep (lua-token) (parse-many (parse-pred #'lua-separator))) (stream-from-str str)))

(defun parse-num()
  "Синтаксический разбор числа"
  (parse-pred #'integerp))

(defun parse-var()
  "Синтаксический разбор переменной"
  (parse-pred #'symbolp))

(defun parse-exp())

(defun parse-factor()
  "Синтаксический разбор множителя"
  (parse-or (parse-num)
	    (parse-var)
	    (&&& (parse-elem #\( ) (parse-rec (parse-exp)) (parse-elem #\) ))))

(defun parse-term()
  "Синтаксический разбор слагаемого"
  (&&& (parse-factor) (parse-many (parse-or
	 (&&& (parse-elem #\*) (parse-factor))
	 (&&& (parse-elem #\/) (parse-factor))))))

(defun parse-exp()
  "Синтаксический разбор выражения"
  (&&& (parse-term) (parse-many (parse-or
	 (&&& (parse-elem #\+) (parse-term))
	 (&&& (parse-elem #\-) (parse-term))))))

(defun parse-stat()
  "Синтаксический разбор оператора"
  (&&& (parse-var)
       (parse-elem #\=)
       (parse-exp)))

(defun parse-block()
  "Синтаксический разбор блока"
  (parse-many (parse-stat)))








