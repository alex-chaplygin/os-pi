"-------------TEST-------------"
(let ((temp (make-huff)))
  ;;Добавление в пустую таблицу "01" со значением 10
  (setq temp (huff-add temp 1 2 10))
  (print (assert (tree-get-val (right-tree (left-tree temp))) 10))
  ;;Добавление в таблицу "001" со значением 20
  (setq temp (huff-add temp 1 3 20))
  ;;Добавление в таблицу "1" со значением 'obj
  (setq temp (huff-add temp 1 1 'obj))
  ;;Добавление в таблицу "00010" со значением 'g
  (setq temp (huff-add temp 2 5 'g))
  (print (assert (tree-get-val (right-tree (left-tree (left-tree temp)))) 20))
  (print (assert (tree-get-val (right-tree temp)) 'obj))
  (print (assert (tree-get-val (left-tree (right-tree (left-tree (left-tree (left-tree temp)))))) 'g))
  (print temp)
  ;;Тестирование чтения значения из таблицы по битовому потоку 01001001 00010001
  (set-bin-src #(73 17))
  (set-big-endian)
  (print (assert (huff-decode temp) 10))
  (print (assert (huff-decode temp) 20))
  (print (assert (huff-decode temp) 20))
  (print (assert (huff-decode temp) 'g))
  (assert (huff-decode temp) 20))
