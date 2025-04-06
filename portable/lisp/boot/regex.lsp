; Библиотека регулярных выражений

; Функции обработки шаблона регулярного выражения
(defun check-correctness(regex)
  "Проверяет регулярное выражение на корректность, при необходимости выводит ошибку с описанием проблемы"
  (let ((quantifiers (list #\* #\+ #\?))
	(inside-brackets NIL))
    (when (= regex "")
      (error "Empty regex"))
    (for i 0 (string-size regex)
	 (if (= (char regex i) #\%)
	     (if (= (+ i 1) (string-size regex))
		 (error "Missing escaped character after %")
	     (incf i)) ; пропускаем рассмотрение экранированного символа
	   (progn
	     (when (and (= (char regex i) #\^) (> i 0))
	       (error "Unescaped ^ not in the beginning of regex"))
	     (when (and (= (char regex i) #\$) (< i (- (string-size regex) 1)))
	       (error "Unescaped $ not in the end of regex"))
	     (when (and (< i (- (string-size regex) 1)) (contains quantifiers (char regex i)) (contains quantifiers (char regex (+ i 1))))
	       (error (concat (make-string 1 (char regex (+ i 1))) " Preceding token is not quantifiable")))
	     (when (= (char regex i) #\[)
	       (if (= inside-brackets NIL)
		   (setq inside-brackets T)
		 (error "Nested square brackets"))
	       (when (and (< i (- (string-size regex) 1)) (= (char regex (+ i 1)) #\]))
		 (error "Empty square brackets")))
	     (when (= (char regex i) #\])
	       (if inside-brackets
		   (setq inside-brackets NIL)
		 (error "Missing opening square bracket"))))))
  (if inside-brackets
      (error "Missing closing square bracket")
      T)))

(defun match-auto(prepared-auto string)
  "Определяет, соответствует ли строка недетерминированному автомату, возвращая захваченную строку или NIL"
  (let* ((auto-params (car prepared-auto))
	 (match-str "")
	(nfa (make-nfa (car auto-params) (cadr auto-params) (caddr auto-params)))
	 (finished NIL)
	 (break-after-match NIL))
    (for i -1 (string-size string) ; + проверка на завершение автомата на пустую строку
	 (when (or (= finished NIL) (and (cadddr prepared-auto) (= break-after-match NIL)))
	   (when (>= i 0)
	     (setq nfa (nfa-input nfa (char string i)))
	     (setq match-str (concat match-str (make-string 1 (char string i)))))
	   ; проверка автомата разрешена только если либо нет поиска с конца строки, либо на последнем символе 
	   (if (and (or (= (caddr prepared-auto) NIL)
			(and (caddr prepared-auto) (= i (- (string-size string) 1))))
		    (nfa-end nfa)) ; автомат подошёл - завершаем работу
	       (setq finished T)
	     (progn ; выполнить последовательность действий в списке
	       (if (and finished (cadddr prepared-auto))
		   (progn
		     (setq break-after-match T)
		     (setq match-str (subseq match-str 0 (- (string-size match-str) 1))))
	       (when (= (nfa-states nfa) NIL) ; автомат разрушился - сбрасывем его и пробуем заново, иначе продолжаем работу
		 (if (cadr prepared-auto)
		     (setq nfa (nfa-reset nfa NIL)) ; если поиск с начала - делаем пустой автомат
		     (setq nfa (nfa-reset nfa (car auto-params))))
		 (setq match-str "")))))))
    (if finished match-str NIL)))

(defun prepare-parsed-auto (parsed-list lazy)
  "Создаёт набор параметров и правил для создания НКА, соответствующего регулярному выражению, принимая на вход список с результатом парсинга шаблона"
  (let ((rules-list NIL)
	(parsed-re (car parsed-list))
	(curr-state 0))
    (labels ((add-rule (current-state input next-states)
	     (if (atom input) ; если на вход подан символ (не список)
		 (setq rules-list (cons (list current-state input next-states) rules-list))
	       (if (= (car input) 'ESC)
		   (setq rules-list (cons (list current-state (cdr input) next-states) rules-list))
	       (dolist (chr input)
		 (if (and (not (atom chr)) (= (car chr) 'ESC))
		     (setq rules-list (cons (list current-state (cdr chr) next-states) rules-list))
		   (setq rules-list (cons (list current-state chr next-states) rules-list))))))))
    (dolist (elem parsed-re)
      (cond
       ((atom elem)
	(add-rule curr-state elem (list (incf curr-state))))
       ((= (car elem) 'ESC)
	(add-rule curr-state (cdr elem) (list (incf curr-state))))
       ((= (car elem) 'MANY)
	(add-rule curr-state 'E (list (incf curr-state)))
	  (add-rule curr-state (cdr elem) (list curr-state)))
       ((= (car elem) 'MANY+)
	  (add-rule curr-state (cdr elem) (list (incf curr-state)))
	  (add-rule curr-state (cdr elem) (list curr-state)))
       ((= (car elem) 'MAYBE)
	(add-rule curr-state 'E (list (+ curr-state 1)))
	(add-rule curr-state (cdr elem) (list (incf curr-state))))
       ((add-rule curr-state elem (list (incf curr-state))))))
    (list
     (list (list 0) (reverse rules-list) (list curr-state))
     (cadr parsed-list)
     (caddr parsed-list)
     (not lazy)))))

(defun parse-escape (lst)
  "Парсит экранированные символы из списка печатных символов шаблона регулярного выражения"
  "Подразумевается, что шаблон проверен на корректность"
  (let ((new-lst NIL))
    (for i 0 (list-length lst)
	 (if (= (nth lst i) #\%)
	     (progn
	      (setq new-lst (cons (cons 'ESC (nth lst (+ i 1))) new-lst))
	      (incf i))
	   (setq new-lst (cons (nth lst i) new-lst))))
    (reverse new-lst)))

(defun parse-brackets-any (lst)
  "Парсит группы символов в квадратных скобках, а также метасимвол ."
  "Метасимволы в квадратных скобок, помимо самих скобок, не имеют силы, поэтому воспринимаются буквально (так же, как и в регулярных выражениях в других языках)"
  (let ((new-lst NIL))
    (for i 0 (list-length lst)
	 (if (= (nth lst i) #\[)
	     (progn
	       (let ((group NIL))
		 (incf i)
		 (while (not (= (nth lst i) #\]))
		   (setq group (cons (nth lst i) group))
		   (incf i))
		 (setq new-lst (cons group new-lst))))
	   (if (= (nth lst i) #\.)
	       (setq new-lst (cons 'ANY new-lst))
	     (setq new-lst (cons (nth lst i) new-lst)))))
    (reverse new-lst)))

(defun parse-quantifiers (lst)
  "Парсит метасимволы-квантификаторы (* + ?)"
  (let ((list-len (list-length lst))
	(new-lst NIL)
	(curr-elem NIL))
    (labels ((n-from-end (n)
	       (nth lst (- list-len n 1))))
	    (for i 0 list-len
		 (setq curr-elem (n-from-end i))
		       (cond
			((= curr-elem #\*)
			 (setq new-lst (cons (cons 'MANY (n-from-end (incf i))) new-lst)))
			((= curr-elem #\+)
			 (setq new-lst (cons (cons 'MANY+ (n-from-end (incf i))) new-lst)))
			((= curr-elem #\?)
			 (setq new-lst (cons (cons 'MAYBE (n-from-end (incf i))) new-lst)))
			((setq new-lst (cons curr-elem new-lst)))))
	    new-lst)))

(defun parse-flags (lst)
  "Парсит метасимволы ^ и $, обозначающие поиск с начала и с конца строки"
  "Возвращает список, состоящий из исходного списка, а также двух флагов"
  (labels ((remove-last (ls)
	  (if (not (cdr ls))
	      NIL
	    (cons (car ls) (remove-last (cdr ls))))))
  (let ((from-start NIL)
	(from-end NIL))
    (when (= (car lst) #\^)
      (setq from-start T)
      (setq lst (cdr lst)))
    (when (and lst (= (nth lst (- (list-length lst) 1)) #\$))
      (setq from-end T)
      (setq lst (remove-last lst)))
    (list lst from-start from-end))))

; Функции обработчика регулярных выражений
(defun match(string regex &rest lazy)
  "Определяет, соответствует ли строка регулярному выражению, возвращая захваченную подстроку или NIL"
  "Третий необязательный параметр - переключение в ленивый режим захвата (по умолчанию жадный)"
  (when (check-correctness regex)
    (match-auto (prepare-parsed-auto (parse-flags (parse-quantifiers (parse-brackets-any (parse-escape (explode regex))))) (if lazy (car lazy) NIL)) string)))

(defun test-re(string regex)
  "Определяет, соответствует ли строка регулярному выражению, возвращая T/NIL"
  (when (check-correctness regex)
    (if (not (= (match-auto (prepare-parsed-auto (parse-flags (parse-quantifiers (parse-brackets-any (parse-escape (explode regex))))) T) string) NIL))
	T
      NIL)))


;; ; Вспомогательные функции
;; (defun reverse-list(lst)
;;   "Переворачивает список"
;;   (foldl #'(lambda (acc elem) (cons elem acc)) NIL lst))

;; (defun remove-dupl(lst)
;;   "Удаляет повторяющиеся элементы из списка"
;;   (let ((unique NIL))
;;     (dolist (elem lst)
;;       (when (not (contains unique elem))
;; 	(setq unique (cons elem unique)))) ; элементы добавляются в начало списка
;;     (reverse-list unique))) ; переворачиваем список обратно
