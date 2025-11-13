;;Тестирование функций для строк
(unit-test 'str)
(defun test-search (ch str res)
  (print (assert (search ch str) res)))

(defun test-search-back (ch str res)
  (print (assert (search-back ch str) res)))

(defun test-split (ch str res)
  (print (assert (split ch str) res)))

(defun test-implode (arr res)
  (print (assert (implode arr) res)))

(defun test-explode (str res)
  (print (assert (explode str) res)))

(defun test-is-alpha (sym res)
  (print (assert (is-alpha sym) res)))

(defun test-strtoint (str base res)
  (print (assert (strtoint str base) res)))

(deftest t-search ()
  "Поиск в строке"
  (test-search #\c "a0cbbbcbcbcbcbcbc" 2)
  (test-search #\ "a0cbb bcbcbcbcbcbc" 5)
  (test-search #\c "" nil)
  (test-search #\g "a0cbbbcbcbcbcbcbc" nil)
  (test-search #\c "c0cbbbcbcbcbcbcbc" 0)
  (test-search #\c "sdfsdfsddsfdsc" 13))

(deftest t-search-back ()
  "Обратный поиск в строке"
  (test-search-back #\d "abvhdhgnvmdhvbtyurd" 18)
  (test-search-back #\ "ab vhdhgnvmdhvbtyur d" 19)
  (test-search-back #\d "" nil)
  (test-search-back #\z "abvhdhgnvmdhvbtyurd" nil)
  (test-search-back #\d "dabvhhgnvmhvbtyur" 0))

(deftest t-split ()
  "Разбиение строки"
  (test-split #\a "babab" '("b" "b" "b"))
  (test-split #\a " a a " '(" " " " " "))
  (test-split #\ "b b b" '("b" "b" "b"))
  (test-split #\a " " '(" "))
  (test-split #\a "" '(""))
  (test-split #\1 "dbfdb1bfbdfb11dsbds" '("dbfdb" "bfbdfb" "" "dsbds")))

(deftest t-implode ()
  "Список в строку"
  (test-implode '(#\a #\b #\c) "abc")
  (test-implode '(#\1 #\b #\c) "1bc")
  (test-implode '(#\ #\b #\c) " bc")
  (test-implode '() ""))

(deftest t-explode ()
  "Строка в список"
  (test-explode "abcd" '(#\a #\b #\c #\d))
  (test-explode "" '())
  (test-explode "a" '(#\a))
  (test-explode " a" '(#\ #\a)))

(deftest t-is-alpha ()
  "Проверка на букву"
  (test-is-alpha #\A t)
  (test-is-alpha #\a t)
  (test-is-alpha #\0 nil))

(deftest t-strtoint ()
  "Строка в целое число"
  (test-strtoint  "1010" 2 10)
  (test-strtoint "1F" 16 31)
  (test-strtoint "123" 10 123)
  (test-strtoint "10" 8 8)
  (test-strtoint "A" 16 10)
  (test-strtoint "Z" 36 35))

(deftest t-strtofloat ()
  "Строка в целое число с плавающей точкой"
  (print (assert-float (strtofloat "123.0") 123.0))
  (print (assert-float (strtofloat "123.") 123.0))
  (print (assert-float (strtofloat "123.5") 123.5))
  (print (assert-float (strtofloat "0.625") 0.625))
  (print (assert-float (strtofloat ".125") 0.125)))

(run-tests)
