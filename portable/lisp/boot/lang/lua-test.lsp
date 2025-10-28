(unit-tests 'lua)

(deftest parse-var-test()
  "Выполняет тест парсинга лексемы переменной"
  (print (assertcar (funcall (parse-var) (stream-from-str "ABCDF")) 'abcdf)))

(run-tests)
