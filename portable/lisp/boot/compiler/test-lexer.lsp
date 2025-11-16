(unit-tests 'lexer)

(deftest full-lexer-test ()
  "Полный тест лексера"
  (print (assert (lisp-lexer 
		  (concat "(123 0xFF "
			  "#\\q"
			  " 12.0625 .125 13. "
                          "ab+-*/=_&|<>%!^~"
			  "'VAR "
			  "`(,a ,@b) "
			  "\"string\"       "
			  " #(1) (1 . 2) "
			  " #'F)"
			  ))
		 '(#\( 123 255
		   #\q
		   12.0625 0.125 13.0
		   AB+-*/=_&|<>%!^~ 
                   QUOTE VAR BACKQUOTE 
                   #\( COMMA A COMMA-AT B #\) 
                   "string" 
                   SHARP #\( 1 #\) 
                   #\( 1 #\. 2 #\) 
                   FUNCTION F #\)
		   )
		 )))

(run-tests)
