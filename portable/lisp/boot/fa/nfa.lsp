; Библиотека функций недетерминированного конечного автомата (НКА)

;; Класс недетерминированного конечного автомата
(defclass Nfa ()
  (cur-state ; текущее состояние
   rules ; правила
   final-states ; конечные состояния
   ))



(defconst *any-char* 'ANY) ; метасимвол, обозначающий любой печатный символ
(defconst *epsilon* 'E) ; метасимвол, обозначающий безусловный (эпсилон) переход

(defun process-epsilon (states rules)
  "Рекурсивно извлекает следующие за эпсилон-переходами состояния и возвращает их"
  "Ввод: список состояний, список правил НКА"
  "Вывод: состояния, следующие за эпсилон переходами, для обычных переходов не возвращается ничего"
  (let ((new-states NIL))
    (dolist (state states)
      (let ((epsilon-state (cons state *epsilon*)))
      (when (check-key rules epsilon-state)
	(setq new-states (append new-states (get-hash rules epsilon-state))) ; если следующее состояние не эпсилон, оно не возвратится - добавляем заранее(дубликаты состояний удалятся)
	(setq new-states (append new-states (process-epsilon (get-hash rules epsilon-state) rules)))))) ; получаем следующие за эпсилон-переходом состояния
    new-states))

(defun make-nfa(start-state rules final-states)
  "Задаёт недетерминированный автомат"
  "Ввод: начальное состояние, список правил, список конечных состояний"
  "Вывод: объект НКА"
  "Список начальных состояний - список из символов всех состояний, которые нужно проанализировать"
  "Список правил - список вида (пара = список_состояний), например (s1.a = (s1 s3 s4))"
  (let ((nfa (make-hash)) ; создаём хэш-таблицу для автомата
	(start-states (list start-state))) 
    (dolist (rule rules)
      (let* ((current-state (car rule))  ; атом
             (input (cadr rule))         ; атом
             (next-states (caddr rule))  ; список
	     (pair (cons current-state input)))
	(if (check-key nfa pair) ; если ключ уже есть в списке - объединяем состояния
	    (set-hash nfa pair (append (get-hash nfa pair) next-states))
	    (set-hash nfa pair next-states))))
    (list (append start-states (process-epsilon start-states nfa)) nfa final-states)))


(defun nfa-input(auto input)
  "Добавляет символ на ленту автомата"
  "Ввод: объект НКА, символ"
  "Вывод: объект НКА"

  "Если автомат уже находится в конечном состоянии, возвращаем автомат без изменений"
  (if (nfa-end auto) auto
    (let* ((start-states (car auto))    ; список начальных состояний
	   (rules (cadr auto))          ; хэш-таблица правил
	   (final-states (caddr auto))  ; список конечных состояний
	   (curr-states (append start-states (process-epsilon start-states rules)))
	   ;; Собираем все следующие состояния с помощью foldl
	   (new-states (foldl #'(lambda (acc state)
				  (let* ((key (cons state input))
					 (anychar-key (cons state *any-char*))
					 (transitions-by-input (if (check-key rules key) (get-hash rules key) nil))
					 (transitions-by-anychar (if (check-key rules anychar-key) (get-hash rules anychar-key) nil))
					 (all-transitions (append transitions-by-input transitions-by-anychar)))
				    (append acc all-transitions)))
			      nil  
			      curr-states)))
      (list (remove-dupl new-states) rules final-states))))

(defun nfa-end(auto)
  "Возвращает вердикт, соответствует ли строка автомату"
  "Ввод: объект НКА"
  "Вывод: булево (T/NIL)"
  (not (null (filter #'(lambda (state) 
			 (not (null (filter #'(lambda (f) (equal f state)) (third auto)))))
		     (car auto)))))

(defun nfa-states(auto)
  "Вывод списка текущих состояний автомата (можно использовать в процессе передачи каждого символа на ленту автомата, чтобы визуализировать процесс)"
  (car auto))
