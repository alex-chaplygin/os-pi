; Абстрактный тип - бинарное дерево
(defconst +node-size+ 3) ; размер списка у узла

(defun correct-tree (t1)
  "Проверка узла t1 на корректность"
  "nil - корректное дерево"
  "Узел и лист корректные, если они состоят из трех элементов"
  (or (null t1) (eq (car t1) 'leaf) (= (list-length t1) +node-size+)))

(defun make-tree (t1 t2 val)
  "Создать дерево с поддеревом t1 - слева, t2 - справа"
  "Элементами дерева могут являться узлы и листья"
"node (left right val) - узел, первый и второй элементы являются соответствующими поддеревьями, третий элемент - значение узла, которое может быть nil или любым объектом"
(if (and (correct-tree t1) (correct-tree t2))
      (list t1 t2 val)
    (error "make-tree: try to make an incorrect node")))

(defun make-leaf (val)
  "Создать лист со значением val"
  "leaf (nil nil val) - лист, не имеет поддеревьев, третий элемент значение узла, которое может быть nil или любым объектом"
  (list 'leaf val))

(defun is-leaf (t1)
  "Является ли t1 листом"
  (if t1 (and (correct-tree t1) (eq (car t1) 'leaf))
    (raise 'invalid-tree 0)))

(defun left-tree (t1)
  "Получение левого поддерева узла t1"
  "Доступен только узлу"
  (if (and (correct-tree t1) (not (is-leaf t1)))
      (car t1)
      (error "left-tree: invalid operation")))

(defun right-tree (t1)
  "Получение правого поддерева узла t1"
  "Доступен только узлу"
  (if (and (correct-tree t1) (not (is-leaf t1)))
    (cadr t1)
    (error "right-tree: invalid operation")))

(defun tree-get-val (t1)
  "Получение значения узла t1"
  (if (correct-tree t1)
   (if (is-leaf t1)
       (cadr t1)
       (caddr t1))
   (error "tree-get-val: try to get val of an incorrect obj")))
