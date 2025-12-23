(defclass Nfa ()
  (cur-state ; текущее состояние
   rules ; правила
   final-states ; конечные состояния
   ))


(defconst *any-char* 'ANY) ; метасимвол, обозначающий любой печатный символ
(defconst *epsilon* 'E) ; метасимвол, обозначающий безусловный (эпсилон) переход
(defconst *pred* 'PRED) ; метасимвол, обозначающий переход с предикатом

(defun process-epsilon (states rules)
  "Рекурсивно извлекает следующие за эпсилон-переходами состояния и возвращает их"
  "Ввод: список состояний, список правил НКА"
  "Вывод: состояния, следующие за эпсилон переходами, для обычных переходов не возвращается ничего"
  (let ((new-states nil))
    (labels ((process (list acc)
	       (if (null list) acc
		   (let* ((state (car list))
			  (epsilon-state (cons state *epsilon*)))
		     (if (or (contains new-states state) (not (check-key rules epsilon-state)))
			 (process (cdr list) acc)
			 (let ((targets (get-hash rules epsilon-state)))
			   (setq new-states (append new-states `(,state)))
			   (process (append targets (cdr list)) (append acc targets))))))))
      (process states '()))))

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


(defun make-backtrack-nfa(start-state rules final-states groups preds)
  "Задаёт недетерминированный автомат для поиска с возвратом"
  "Ввод: начальное состояние, список правил, список конечных состояний, список групп, список предикатов"
  "Вывод: объект НКА"
  "Список начальных состояний - список из символов всех состояний, которые нужно проанализировать"
  "Список правил - список вида (состояние метасимвол_перехода (список_состояний)), например (s1 a (s1 s3 s4))"
  (let ((nfa (make-hash)) ; создаём хэш-таблицу для автомата
	(start-states (list start-state))
	(groups-hash (make-hash))
	(preds-hash (make-hash))
	(group-number 0)) 
    (dolist (rule rules)
      (let* ((current-state (car rule))  ; атом
             (input (cadr rule))         ; атом
             (next-states (caddr rule))  ; список
	     (pair (cons current-state input)))
	(if (check-key nfa pair) ; если ключ уже есть в списке - объединяем состояния
	    (set-hash nfa pair (append (get-hash nfa pair) next-states))
	    (set-hash nfa pair next-states))))
    (dolist (group groups)
      (let ((start (car group))
	    (end (second group)))
	(set-hash groups-hash start `(start ,group-number))
	(set-hash groups-hash end `(end ,group-number))
	(setq group-number (+ group-number 1))))
    (dolist (pred preds)
	(set-hash preds-hash (car pred) (second pred)))
    (list start-states nfa final-states groups-hash preds-hash)))


(defun nfa-input(auto input)
  "Добавляет символ на ленту автомата"
  "Ввод: объект НКА, символ"
  "Вывод: объект НКА"
  (let* ((start-states (car auto))     ; список начальных состояний
	 (rules (cadr auto))           ; хэш-таблица правил
	 (final-states (caddr auto)))  ; список конечных состояний
    (labels ((process (list acc)
	       (if (null list) acc
		   (let* ((state (car list))
			  (key (cons state input))
			  (anychar-key (cons state *any-char*))
			  (transitions-by-input (if (check-key rules key) (get-hash rules key) nil))
			  (transitions-by-anychar (if (check-key rules anychar-key) (get-hash rules anychar-key) nil))
			  (all-transitions (append transitions-by-input transitions-by-anychar)))
		     (process (cdr list) (append all-transitions acc))))))
      (let* ((new-states (process start-states '())))
	(list (append new-states (process-epsilon new-states rules)) rules final-states)))))

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
