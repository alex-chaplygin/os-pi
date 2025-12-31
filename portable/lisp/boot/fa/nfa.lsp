; Библиотека функций недетерминированного конечного автомата (НКА)
;; Класс недетерминированного конечного автомата
(defclass Nfa ()
  (cur-state ; текущее состояние (множество состояний)
   rules     ; правила (хэш-таблица)
   final-states ; конечные состояния (множество)
   ))

(defconst *any-char* 'ANY) ; метасимвол, обозначающий любой печатный символ
(defconst *epsilon* 'E) ; метасимвол, обозначающий безусловный (эпсилон) переход

(defun process-epsilon (states rules)
  "Рекурсивно извлекает все состояния, достижимые по эпсилон-переходам, с использованием множеств"
  (if (empty-set states) (make-set)
      (let* ((state (car states))  
             (rest-states (cdr states)) 
             (epsilon-key (cons state *epsilon*))
             (direct-states (if (check-key rules epsilon-key)
                                (list-to-set (get-hash rules epsilon-key))
                              (make-set))))
        (let ((recursive-states (process-epsilon direct-states rules))
              (rest-result (process-epsilon rest-states rules)))
          (set-union (set-union direct-states recursive-states) rest-result)))))

(defun build-nfa (start-state rules final-states)
  "Создаёт недетерминированный автомат с использованием множеств и возвращает объект класса Nfa"
  (let ((nfa-hash (make-hash))
        (start-states (set-insert (make-set) start-state)))
    (dolist (rule rules)
      (let* ((current-state (car rule))
             (input (cadr rule))
             (next-states (list-to-set (caddr rule)))
             (pair (cons current-state input)))
        (if (check-key nfa-hash pair)
            (set-hash nfa-hash pair (set-union (get-hash nfa-hash pair) next-states))
            (set-hash nfa-hash pair next-states))))
    (let ((epsilon-states (process-epsilon start-states nfa-hash)))
      (let ((initial-states (set-union start-states epsilon-states))
            (final-set (list-to-set final-states)))
        (make-Nfa initial-states nfa-hash final-set)))))

(defun nfa-input (auto input)
  "Добавляет символ на ленту автомата"
  "Ввод: объект НКА, символ"
  "Вывод: новый объект НКА"  
  "Если автомат уже находится в конечном состоянии, возвращаем автомат без изменений"
  (if (nfa-end auto) auto
    (let* ((current-states (Nfa-cur-state auto))
           (rules (Nfa-rules auto))
           (final-states (Nfa-final-states auto))
           ;; Собираем все следующие состояния через переходы по символу
           (next-states (foldl
                         #'(lambda (acc state)
                             (let* ((key (cons state input))
                                    (anychar-key (cons state *any-char*))
                                    (transitions-by-input 
                                     (if (check-key rules key) 
                                         (list-to-set (get-hash rules key)) 
                                       (make-set)))
                                    (transitions-by-anychar 
                                     (if (check-key rules anychar-key) 
                                         (list-to-set (get-hash rules anychar-key)) 
                                       (make-set))))
                               (set-union acc (set-union transitions-by-input transitions-by-anychar))))
                         (make-set)
                         current-states))
           (epsilon-closure (process-epsilon next-states rules))
           (final-states-after-input (set-union next-states epsilon-closure)))
      (make-Nfa final-states-after-input rules final-states))))

(defun nfa-end (auto)
  "Возвращает T, если автомат находится в конечном состоянии"
  (not (empty-set (set-intersect (Nfa-cur-state auto) (Nfa-final-states auto)))))

(defun nfa-states (auto)
  "Возвращает текущие состояния автомата"
  (Nfa-cur-state auto))
