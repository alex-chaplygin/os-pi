(defun parse-suc (val)
  "Элементарный парсер - успешный разбор со значением val"
  #'(lambda (list) (list (cons val list))))

(defun parse-fail ()
  "Элементарный парсер - неудачный разбор"
  #'(lambda (list) nil))

(defun parse-pred (pred)
  "Парсер по предикату, предикат - функция, которая на вход получает символ, на выходе - nil или t.
   Сама функция парсинга возвращает в результате парсинга в случае успешного разбора сам символ, в случае неудачного - nil."
  #'(lambda (list)
      (if (null list) nil
	  (if (funcall pred (car list)) (list list) nil))))

(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент в списке"
  (parse-pred #'(lambda (x) (= x sym))))

(defun &&& (&rest parsers)
  "Последовательный комбинатор применяет несколько парсеров подряд к списку, каждый следующий parser применяется к остатку от работы предыдущего parser."
  ;; (&&& (parse-or (parse-suc 'A) (parse-suc 'B))
  ;;      (parse-or (parse-suc 'C) (parse-suc 'D))
  ;; 	  (parse-elem 'C)) '(C D)
  ;; 1. (parse-or (parse-suc 'A) (parse-suc 'B)) '(C D)
  ;;       ((A . (C D)) (B . (C D)))
  ;; 2. (parse-or (parse-suc 'C) (parse-suc 'D)) '((A . (C D)) (B . (C D)))
  ;;     ((C . (C D)) (D . (C D)))
  ;;     ((((A C) . (C D)) ((A D) . (C D))) (((B C) . (C D)) ((B D) . (C D))))
  ;;    concatl
  ;; (((A C) . (C D)) ((A D) . (C D)) ((B C) . (C D)) ((B D) . (C D)))
  #'(lambda (list)
      (labels (; генерация результатов парсера p от списка - первый список
	       (gen-parser (p list) 
		 (if (null list) nil (funcall p list)))
	       (merge-result (r1 res2) ; соединить результат r1 в список r2
		 (print `(merge ,r1 ,res2))
		 (map #'(lambda (r2)
			  (cons (append (if (pairp r1) r1 (list r1)) (list (car r2)))
				(cdr r2)))
		      res2))
	       (app-parsers (parsers res) ; применить список парсеров после результата
		 (print `(app ,res))
		 (if (null res) nil
		     (if (null parsers) res
			 (app-parsers (cdr parsers)
				      (concatl (map #'(lambda (r)
							(merge-result (car r)
								      (funcall (car parsers) (cdr r))))
						    res)))))))
	(app-parsers (cdr parsers) (gen-parser (car parsers) list)))))

(defun parse-or (&rest parsers)
  "Параллельный комбинатор принимает список парсеров parsers, объединяя результаты разбора всех парсеров."
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (list)
      (labels ((apply-parser (parsers list res)
		 (if (null parsers) res
		     (let ((parser-res (funcall (car parsers) list)))
		       (apply-parser (cdr parsers) list (append res parser-res))))))
      (apply-parser parsers list nil))))
		     
(defun parse-app (parser f)
  "Комбинатор применения функции ко всем результатам разбора"
  #'(lambda (list)
      (let ((res (funcall parser list)))
	(map #'(lambda (r) (cons (funcall f (car r)) (cdr r))) res))))

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений заданного парсера. Возвращает список результатов"
  ;; (parse-many (parse-elem 'A)) '(A A B) -> ((NIL A A B) ((A) A B) ((A A) B))
  ;; parser (A A B) NIL -> ((A) A B)
  ;; parser (A B) (A) -> ((A A) B)
  ;; parser (B) (A A) -> (A A)
  (parse-or (parse-suc nil)
  #'(lambda (list)
      (labels ((apply (list res)
		 (if (null list) (list (list res))
		     (let ((parser-res (funcall parser list)))
		       (if (null parser-res)
			   (list (cons res list))
			   (apply (cdar parser-res) (append res (list (caar parser-res)))))))))
	(apply list nil))))

(defun parse-some (parser)
  "Комбинатор - 1 или более повторений заданного парсера. Возвращает список результатов"
  (parse-app (&&& parser (parse-many parser))
	     #'(lambda (x) (cons (car x) (second x)))))

(defun parser-value (parser str)
  "Вернуть значение результата разбора строки str парсером parser"
  (caar (funcall parser (explode str))))

(defun skip-spaces ()
  "Пропуск 0 или более пробелов"
  (parse-many (parse-elem #\ )))
