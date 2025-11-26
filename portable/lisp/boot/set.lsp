(defun make-set ()
  "Создает пустое множество."
  nil)

(defun empty-set (set)
  "Проверяет, является ли множество пустым"
  (null set))

(defun set-member (element set)
  "Проверяет, содержится ли элемент в множестве"
  (cond
    ((null set) nil)
    ((equal element (car set)) t)
    (t (set-member element (cdr set)))))

(defun set-insert (set element)
  "Добавляет элемент в множество, если его там еще нет"
  (if (set-member element set) set (cons element set)))

(defun set-remove (element set)
  "Удаляет элемент из множества"
  (cond
    ((null set) nil)
    ((equal element (car set)) (cdr set))
    (t (cons (car set) (set-remove element (cdr set))))))

(defun set-union (set1 set2)
  "Объединяет два множества"
  (cond
    ((null set1) set2)
    ((null set2) set1)
    ((set-member (car set1) set2) (set-union (cdr set1) set2))
    (t (cons (car set1) (set-union (cdr set1) set2)))))

(defun set-intersect (set1 set2)
  "Возвращает пересечение двух множеств"
  (cond
    ((null set1) nil)
    ((set-member (car set1) set2) (cons (car set1) (set-intersect (cdr set1) set2)))
    (t (set-intersect (cdr set1) set2))))

(defun list-to-set (lst)
  "Преобразует список во множество(внося только уникальные элементы)"
  (cond
    ((null lst) nil)
    ((set-member (car lst) (cdr lst)) (list-to-set (cdr lst)))
    (t (cons (car lst) (list-to-set (cdr lst))))))

(defun set-size (set)
  "Возвращает количество элементов в множестве"
  (if (null set) 0 (+ 1 (set-size (cdr set)))))
