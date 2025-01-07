; Вспомогательные функции
(defun reverse-list(lst)
  "Переворачивает список"
  (foldl #'(lambda (acc elem) (cons elem acc)) NIL lst))

(defun remove-dupl(list)
  "Удаляет повторяющиеся элементы из списка"
  (let ((unique NIL))
    (dolist (elem list)
      (when (not (contains unique elem))
	(setq unique (cons elem unique)))) ; элементы добавляются в начало списка
    (reverse-list unique))) ; переворачиваем список обратно


; Высокоуровневые функции обработчика регулярных выражений
(defun match(string regex &rest lazy)
  "Определяет, соответствует ли строка регулярному выражению, возвращая захваченную строку или NIL"
  (when (check-correctness regex) ; если автомат некорректен, функция сама выведет error и прервёт выисления
    (match-auto (prepare-auto regex (if lazy (car lazy) NIL)) string))) ; rest возвращает список аргументов

(defun test(string regex)
  "Определяет, соответствует ли строка регулярному выражению, возвращая T/NIL"
  (when (check-correctness regex)
    (if (not (= (match-auto (prepare-auto regex T) string) NIL))
	T
      NIL)))


(defun check-correct(regex) ; должно быть check-correctness, временно отключено
  "Проверяет регулярное выражение на корректность"
  (let ((quantifiers (list #\* #\+ #\?))
	(inside-brackets NIL))
    (when (= regex "")
      (error "Empty regex"))
    (for i 0 (string-size regex)
	 (if (= (char regex i) #\%)
	     (incf i) ; пропускаем рассмотрение экранированного символа
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

(defun check-correctness(regex)
  "Проверяет регулярное выражение на корректность" ; базовая проверка, пока интерпретатор не поддрерживает полную
  (when (= regex "")
    (error "Empty regex"))
  T)


(defun prepare-auto(regex lazy)
  "Создаёт набор параметров и правил для создания НКА, соответствующего регулярному выражению"
  (labels ((add-rule (current-state input next-states)
	     (if (atom input) ; если на вход подан символ (не список)
		 (setq rules-list (cons (list current-state input next-states) rules-list))
		 (dolist (chr input)
		   (setq rules-list (cons (list current-state chr next-states) rules-list)))))
	   (calc-for-sym (chr)
	     (if (and (= chr #\]) (not (= bracket-states NIL)))
		 bracket-states
	     (cond
	      (prev-escaped
	       (setq prev-escaped NIL)
	       chr)
	      ((= chr #\.) 'ANY)
	      (T chr))))
	   (process-brackets (idx)
	     (let ((start-idx idx)
		   (states NIL))
	       (incf idx) ; пропуск анализа [
	       (while (not (= (char regex idx) #\]))
		 (when (= (char regex idx) #\%)
		   (incf idx))
		 (setq states (cons (char regex idx) states))
		 (incf idx))
	       (list states (- idx start-idx)))))
   (let ((rules-list NIL)
	 (from-start NIL)
	 (from-end NIL)
	 (curr-state 0)
	 (bracket-states NIL)
	 (prev-escaped NIL))
     (for i 0 (string-size regex)
       (when (= (char regex i) #\%)
	 (setq prev-escaped T)
	 (incf i))
       (when (and (= (char regex i) #\[) (= prev-escaped NIL))
	 (setq result (process-brackets i))
	 (setq bracket-states (car result))
	 (setq i (+ i (cadr result))))
       (cond
	 ((and (< i (- (string-size regex) 1)) (= (char regex (+ i 1)) #\*))
	  (add-rule curr-state 'E (list (incf curr-state)))
	  (add-rule curr-state (calc-for-sym (char regex i)) (list curr-state))
	  (incf i))
	 ((and (< i (- (string-size regex) 1)) (= (char regex (+ i 1)) #\+))
	  (let ((state-rules (calc-for-sym (char regex i))))
	    (add-rule curr-state state-rules (list (incf curr-state)))
	    (add-rule curr-state state-rules (list curr-state))
	    (incf i)))
	 ((and (< i (- (string-size regex) 1)) (= (char regex (+ i 1)) #\?))
	  (add-rule curr-state 'E (list (+ curr-state 1)))
	  (add-rule curr-state (calc-for-sym (char regex i)) (list (incf curr-state)))
	  (incf i))
	 ((and (= (char regex i) #\^) (= i 0)) (setq from-start T))
	 ((and (= (char regex i) #\$) (= prev-escaped NIL) (= i (- (string-size regex) 1))) (setq from-end T))
	 (T (add-rule curr-state (calc-for-sym (char regex i)) (list (incf curr-state))))))

     (list
      (list (list 0) (reverse-list rules-list) (list curr-state))
      from-start
      from-end
      (not lazy)))))

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
