; Библиотека функций детерминированного конечного автомата (ДКА)

(defun make-dfa (start-state rules final-states)
  "Задаёт детерминированный автомат"
  "Ввод: символ начального состояния, список правил, список конечных состояний"
  "Вывод: объект ДКА"
  (let ((dfa (make-hash))) ; Создаем хэш-таблицу для автомата
    (dolist (rule rules) ; Заполняем хэш-таблицу правилами
      (let* ((current-state (car rule))
            (input (cadr rule))
            (next-state (caddr rule)))
	(set-hash dfa (cons current-state input) next-state))) ; Устанавливаем новые переходы
    (list start-state dfa final-states)))

(defun dfa-input (auto input)
  "Добавляет символ на ленту автомата"
  "Ввод: объект ДКА, символ"
  "Вывод: объект ДКА"
  (let* ((state (car auto))
	(rules (cadr auto))
	(final-states (caddr auto))
	(key (cons state input)))
    (setq state (if (check-key rules key)
		    (get-hash rules key) ;переход на новое состояние
		  NIL)) ;автомат разрушается
    (list state rules final-states)))

(defun dfa-end (auto)
  "Возвращает вердикт, соответствует ли строка автомату"
  "Ввод: объект ДКА"
  "Вывод: булево (T/NIL)"
  (let ((state (car auto))
	(final-states (caddr auto)))
    (contains final-states state)))

(defun dfa-reset(auto start-state)
  "Сбрасывает состояние автомата в изначальное (подаваемое на вход функции)"
  (list start-state (cadr auto) (caddr auto)))

(defun dfa-state (auto)
  "Вывод текущего состояния автомата (можно использовать в процессе передачи каждого символа на ленту автомата, чтобы визуализировать процесс)"
  (car auto))
