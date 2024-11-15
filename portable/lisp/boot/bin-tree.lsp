(defconst +node-size+ 3)

(defun correct-tree (t1)
  "Проверка узла t1 на корректность"
  "Узел и лист корректные, если они состоят из трех элементов"
  (if (= (list-length t1) +node-size+) t nil))

(defun make-tree (t1 t2 val)
  "Создать дерево с поддеревом t1 - слева, t2 - справа"
  "Элементами дерева могут являться узлы и листья"
"node (left right val) - узел, первый и второй элементы являются соответствующими поддеревьями, третий элемент - значение узла, которое может быть nil или любым объектом"
  (if (and (correct-tree t1) (correct-tree t2))
      (list t1 t2 val)
    (error '(try to make an incorrect node))))

(defun make-leaf (val)
  "Создать лист со значением val"
  "leaf (nil nil val) - лист, не имеет поддеревьев, третий элемент значение узла, которое может быть nil или любым объектом"
  (list nil nil val))

(defun is-leaf (t1)
  "Является ли t1 листом"
  (if (and (correct-tree t1) (null (car t1)) (null (cadr t1))) t nil))

(defun left-tree (t1)
  "Получение левого поддерева узла t1"
  "Доступен только узлу"
  (if (and (correct-tree t1) (null (is-leaf t1)))
      (car t1)
      (error '(wrong access to left tree))))

(defun right-tree (t1)
  "Полуение правого поддерева узла t1"
  "Доступен только узлу"
  (if (and (correct-tree t1) (null (is-leaf t1)))
    (cadr t1)
    (error '(wrong access to right tree))))

(defun set-left-tree (t1 left)
  "Установка левого поддерева узлу t1"
  "Доступен только узлу"
  (if (and (correct-tree t1) (correct-tree left) (null (is-leaf t1)))))
      

(defun set-right-tree (t1 right)
  "Установка правого поддерева узлу t1"
  "Доступен только узлу"
  )

(defun tree-get-val (t1)
  "Получение значения узла t1"
  )

(defun tree-set-val (t1 val)
  "Установка значения val узлу t1"
  )
