;;Энтропийное кодирование Хаффмана
(defun make-huff ()
  "Создать пустое дерево Хаффмана"
  nil)

(defun huff-add (huff code size val)
  "Добавить в дерево huff значение val длиной size по коду code"
  (if (= size 0)
      (if (null huff)
	  (make-leaf val)
	(error "huff-add: invalid code"))
    (let* ((siz (-- size))
	  (bit (= 0 (& 1 (>> code siz)))))
      (if (null huff)
	  (if bit
	      (make-tree (huff-add nil code siz val) nil nil)
	    (make-tree nil (huff-add nil code siz val) nil))
	(if bit
	    (make-tree (huff-add (left-tree huff) code siz val) (right-tree huff) nil)
	  (make-tree (left-tree huff) (huff-add (right-tree huff) code siz val) nil))))))

(defun huff-decode (huff)
  "Декодирование таблицы huff по данным из потока")
