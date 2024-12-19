(defun parse-element (expected-element)
  "функция, которая создает элементарный парсер, ожидающий заданный элемент в списке"
  #'(lambda (input-list)
    (if (and input-list (equal (car input-list) expected-element))
	(list (cons expected-element (cdr input-list)))
	(list (cons nil input-list)))))
