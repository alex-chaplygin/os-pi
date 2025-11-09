(unit-tests 'lua)

(deftest parse-symbol-test()
  "Выполняет тест парсинга лексемы переменной"
  (print (assertcar (funcall (parse-symbol) (stream-from-str "abcdf")) 'abcdf)))

(deftest parse-numeral-dec-test()
  "Выполняет тест парсинга лексемы числа в десятеричном виде"
  (print (assertcar (funcall (parse-numeral) (stream-from-str "123")) 123)))

(deftest parse-numeral-dec-test()
  "Выполняет тест парсинга лексемы числа в шестнадцетиричном виде"
  (print (assertcar (funcall (parse-numeral) (stream-from-str "0xAF")) 175)))

(deftest lua-lexer-test()
  "Выполняет тест лексического анализатора Lua"
  (print (assertcar (lua-lexer "acd = 1 + 2") '(acd #\= 1 #\+ 2))))

(deftest lua-parse-block-test()
  (print (assertcar (funcall (parse-block) (stream-from-list '(acd #\= 1 #\+ b #\* 10))) '(setq acd (+ 1 2)))))

(run-tests)
