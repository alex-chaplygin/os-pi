;Тестирование функций для строк
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



(test-search #\c "a0cbbbcbcbcbcbcbc" 2)
(test-search #\ "a0cbb bcbcbcbcbcbc" 5)
(test-search #\c "" nil)
(test-search #\g "a0cbbbcbcbcbcbcbc" nil)
(test-search #\c "c0cbbbcbcbcbcbcbc" 0)
(test-search #\c "sdfsdfsddsfdsc" 13)
(print "______________________________________________________________________")

(test-search-back #\d "abvhdhgnvmdhvbtyurd" 18)
(test-search-back #\ "ab vhdhgnvmdhvbtyur d" 19)
(test-search-back #\d "" nil)
(test-search-back #\z "abvhdhgnvmdhvbtyurd" nil)
(test-search-back #\d "dabvhhgnvmhvbtyur" 0)
(print "______________________________________________________________________")

(test-split #\a "babab" '("b" "b" "b"))
(test-split #\a " a a " '(" " " " " "))
(test-split #\ "b b b" '("b" "b" "b"))
(test-split #\a " " '(" "))
(test-split #\a "" '(""))
(test-split #\1 "dbfdb1bfbdfb11dsbds" '("dbfdb" "bfbdfb" "" "dsbds"))
(print "______________________________________________________________________")

(test-implode '(#\a #\b #\c) "abc")
(test-implode '(#\1 #\b #\c) "1bc")
(test-implode '(#\ #\b #\c) " bc")
(test-implode '() "")
(print "______________________________________________________________________")

(test-explode "abcd" '(#\a #\b #\c #\d))
(test-explode "" '())
(test-explode "a" '(#\a))
(test-explode " a" '(#\ #\a))

