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

;(deftest lua-parse-stat-test()
;   (print (car (funcall (parse-args) (stream-from-list
; 		    '(#\( name1 #\, name2 #\, name3 #\)))))))



(deftest test-hash()
  "hash test"
  (setq a 1)
  (setq b 2)
  (lua-set (a b) (b a))
  (print a b)
  (print (lua-createtable (list '("a" "first") '(indexed "second") '(indexed "third") '("b" "fourth")))))

(deftest parse-function-test()
  "Выполняет тест парса функции"
  (print (car (funcall (parse-function) (stream-from-list
		    '(FUNCTION #\( ARG #\) ARG LUA-SET 100 END))))))


(deftest lua-lexer-test()
  "Выполняет тест лексического на Lua в Lisp-выражение"
   (print (car (lua-lexer "
print(a.b.c)
a = { b = '1', c = '2', 3,4 };
abc = 100
end                        
 b = \"aboaasd\"           
 c = false and nil"))))

(deftest lua-test()
  "Выполняет тест преобразования строки на Lua в Lisp-выражение"
   (print (lua-to-lisp "
Person = {}

function Person.new(name, age)
   local p = {}
   p.name = name
   p.age = age
   function p:get_name()
      return self.name
   end

   function p:set_name(name)
      self.name = name
   end

   function p:hello()
      return 'Hello, my name is ' .. self.name .. ', i am ' .. self.age .. ' years old'
   end

   return p
end

vasya = Person.new('Vasya', 30)
kolya = Person.new('Kolya', 20)
vasya, kolya = kolya, vasya
print(vasya:hello())
print(kolya:hello())")))

(deftest lua-progn-test()
  "Выполняет тест исполнения Lisp-выражения, преобразованного из Lua кода"
   (eval (lua-to-lisp
 "
Person = {}

function Person.new(name, age)
   local p = {}
   p.name = name
   p.age = age
   function p:get_name()
      return self.name
   end

   function p:set_name(name)
      self.name = name
   end

   function p:hello()
      return 'Hello, my name is ' .. self.name .. ', i am ' .. self.age .. ' years old'
   end

   return p
end

vasya = Person.new('Vasya', 30)
kolya = Person.new('Kolya', 20)
vasya, kolya = kolya, vasya
print(vasya:hello())
print(kolya:hello())"))
  ; (print `(a = ,a b = ,b c = ,c))
   )

(run-tests)



