; Библиотека функций недетерминированного конечного автомата (НКА)

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

(defun make-nfa(start-states rules final-states)
  "Задаёт недетерминированный автомат"
  "Ввод: список начальных состояний, список правил, список конечных состояний"
  "Вывод: объект НКА"
  "Список начальных состояний - список из символов всех состояний, которые нужно проанализировать"
  "Список правил - список вида (пара = список_состояний), например (s1.a = (s1 s3 s4))"
  (let ((nfa (make-hash))) ; создаём хэш-таблицу для автомата
    (dolist (rule rules)
      (let* ((current-state (car rule))  ; атом
             (input (cadr rule))         ; атом
             (next-states (caddr rule))  ; список
	     (pair (cons current-state input)))
	(if (check-key nfa pair) ; если ключ уже есть в списке - объединяем состояния
	    (set-hash nfa pair (append (get-hash nfa pair) next-states))
	    (set-hash nfa pair next-states))))
    (setq start-states (append start-states (process-epsilon start-states nfa)))
    (list start-states nfa final-states)))

(defun nfa-input(auto input)
  "Добавляет символ на ленту автомата"
  "Ввод: объект НКА, символ"
  "Вывод: объект НКА"
  (let ((start-states (car auto))    ; список
	(rules (cadr auto))          ; хэш-таблица
	(final-states (caddr auto))  ; список
	(curr-states NIL)
	(new-states NIL))
    (setq curr-states (append start-states (process-epsilon start-states rules)))
    (dolist (state curr-states)
      (let ((key (cons state input))
	    (anychar-key (cons state *any-char*)))
	(when (check-key rules key)
	  (setq new-states (append new-states (get-hash rules key))))
	(when (check-key rules anychar-key)
	  (setq new-states (append new-states (get-hash rules anychar-key))))))
    (list (remove-dupl new-states) rules final-states))) ; удаляем состояния-дубликаты

(defun nfa-end(auto)
  "Возвращает вердикт, соответствует ли строка автомату"
  "Ввод: объект НКА"
  "Вывод: булево (T/NIL)"
  (let ((states (car auto))
	(final-states (caddr auto))
	(result NIL))
    (dolist (state states)
      (when (contains final-states state)
	(setq result T)))
    result))

(defun nfa-reset(auto start-states)
  "Сбрасывает состояния автомата в изначальные (подаваемые на вход функции)"
  (list start-states (cadr auto) (caddr auto)))

(defun nfa-states(auto)
  "Вывод списка текущих состояний автомата (можно использовать в процессе передачи каждого символа на ленту автомата, чтобы визуализировать процесс)"
  (car auto))
