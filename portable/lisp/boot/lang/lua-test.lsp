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


(deftest parse-function-test()
  "Выполняет тест парса функции"
  (print (car (funcall (parse-function) (stream-from-list
		    '(FUNCTION #\( ARG #\) ARG LUA-SET 100 END))))))

(deftest lua-lexer-test()
  "Выполняет тест лексического на Lua в Lisp-выражение"
   (print (car (lua-lexer "
a = function (abc)
abc = 100
end                        
 b = \"aboaasd\"           
 c = false and nil"))))

(deftest lua-test()
  "Выполняет тест преобразования строки на Lua в Lisp-выражение"
   (print (lua-to-lisp "
function createMultiplier(factor)
     return function(x)
         a = x * factor
         print(a)
         return a
     end
 end


 multiplyBy5 = createMultiplier(5)
 multiplyBy10 = createMultiplier(10)

 multiplyBy5(90) 
 multiplyBy10(90);

 (function (x)
    print('x * 2 = ' .. x * 2 .. ', x = ' .. x)
 end)(10)

 createMultiplier(10)(100)")))

(deftest lua-progn-test()
  "Выполняет тест исполнения Lisp-выражения, преобразованного из Lua кода"
   (eval (lua-to-lisp
 "function createMultiplier(factor)
     return function(x)
         a = x * factor
         print('x * factor = ' .. a)
         return a
     end
 end


 multiplyBy5 = createMultiplier(5)
 multiplyBy10 = createMultiplier(10)

 multiplyBy5(90) 
 multiplyBy10(90);

 (function (x)
    print('x * 2 = ' .. x * 2 .. ', x = ' .. x)
 end)(10)

 createMultiplier(10)(100)"))
  ; (print `(a = ,a b = ,b c = ,c))
   )

(run-tests)



