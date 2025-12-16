;; Модульные тесты Хаффмана
(unit-tests 'huffman)

(deftest test-make-huff ()
  "Тестирование создания пустого дерева Хаффмана"
  (let ((temp (make-huff)))
    (print (assert temp nil))))

(deftest test-huff-add ()
  "Тестирование добавления в дерево Хаффмана"
  (let ((temp (make-huff)))
    ;; Добавление в пустую таблицу "01" со значением 10
    (setq temp (huff-add temp 1 2 10))
    (print (assert (tree-get-val (right-tree (left-tree temp))) 10))
    ;; Добавление в таблицу "001" со значением 20
    (setq temp (huff-add temp 1 3 20))
    ;; Добавление в таблицу "1" со значением 'obj
    (setq temp (huff-add temp 1 1 'obj))
    ;; Добавление в таблицу "00010" со значением 'g
    (setq temp (huff-add temp 2 5 'g))
    (print (assert (tree-get-val (right-tree (left-tree temp))) 10))
    (print (assert (tree-get-val (right-tree (left-tree (left-tree temp)))) 20))
    (print (assert (tree-get-val (right-tree temp)) 'obj))
    (print (assert (tree-get-val (left-tree (right-tree (left-tree (left-tree (left-tree temp)))))) 'g))))

(deftest test-huff-decode ()
  "Тестирование декодирования данных из двоичного потока"
  (let ((temp (make-huff)))
    (setq temp (huff-add temp 1 2 10))
    (setq temp (huff-add temp 1 3 20))
    (setq temp (huff-add temp 1 1 'obj))
    (setq temp (huff-add temp 2 5 'g))
    ;;Тестирование чтения значения из таблицы по битовому потоку 01001001 00010001
    (let* ((stream (stream-from-arr #(73 17) t))
           (r1 (funcall (huff-decode temp) stream))
           (r2 (funcall (huff-decode temp) (cdr r1)))
           (r3 (funcall (huff-decode temp) (cdr r2)))
           (r4 (funcall (huff-decode temp) (cdr r3)))
           (r5 (funcall (huff-decode temp) (cdr r4))))
      (print (assertcar r1 10))
      (print (assertcar r2 20))
      (print (assertcar r3 20))
      (print (assertcar r4 'g))
      (print (assertcar r5 20)))))

(deftest test-huff-make-code-lens ()
  "Тестирование построения дерева Хаффмана по списку длин кодов"
  
  (let ((huff (huff-make-code-lens #(0 1 5 2) #(f a b c d e g h))))
    (let* ((stream (stream-from-arr #(0x13 0x98) t)) ;; 0001 0011 1001 1000
	   (r1 (funcall (huff-decode huff) stream))
           (r2 (funcall (huff-decode huff) (cdr r1)))
           (r3 (funcall (huff-decode huff) (cdr r2)))
           (r4 (funcall (huff-decode huff) (cdr r3)))
           (r5 (funcall (huff-decode huff) (cdr r4))))
      (print (assertcar r1 'f))
      (print (assertcar r2 'a))
      (print (assertcar r3 'b))
      (print (assertcar r4 'c))
      (print (assertcar r5 'e)))))

(run-tests)
