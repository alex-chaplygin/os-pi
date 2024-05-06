; Библиотека функций для массивов

(defmacro with-struct (struct ar ofs &rest body)
  "Выполнить вычисление body и установить значения переменных структуры struct из массива arr по смещению ofs"
  `(let ,(struct-fields (eval struct) (eval ar) (eval ofs)) ,@body))

(defun struct-fields (struct arr ofs)
  "Преобразовывает структуру в список для let"
					;  (let ((attr (arr-get-num arr ofs num))
					;       ((type .. ))
   (car (foldl '(lambda (acc elem)
	    (let ((list (car acc))
		  (ofs (cdr acc))
		  (field (car elem))
		  (size (cdr elem)))
	      (cons (append list (list
				  (if (eq field 'str)
				      `(,(cadr elem) (arr-get-str ,arr ,ofs ,(cddr elem)))
				      `(,field (arr-get-num ,arr ,ofs ,size)))))
		    (if (eq field 'str) (+ ofs (cdr size)) (+ ofs size)))))
	  (cons nil ofs) struct)))

(defun arr-get-num (arr ofs size)
  "Прочесть из массива arr по смещению ofs size байт"
  (if (equal size 0) 0
    (let ((it (- size 1)))
      (+ (<< (aref arr (+ ofs it)) (<< it 3)) (arr-get-num arr ofs it)))))

(defun arr-set-num (arr ofs val size)
  "Записать в массив arr по смещению ofs значение val занимающее size байт"
    (for i 0 size
	 (seta arr (+ ofs i) (& val 0xff))
	 (setq val (>> val 8))))

(defun arr-get-str (arr ofs size)
  "Прочесть из массива arr по смещению ofs строку размером size"
  (if (equal size 0) ""
      (let ((s ""))
	(for i ofs (+ ofs size)
	     (setf s (concat s (code-char (aref arr i)))))
	s)))

(defun arr-set-str (arr ofs str size)
  "Записать в массив arr по смещению ofs строку str но не превышая размер size"
  (let ((size2 (string-size str)))
    (for i 0 size2
	 (when (< i size) (seta arr (+ i ofs) (char str i))))))

(defun write-struct (arr offs struct values)
  "Записать в массив arr по смещению ofs значения из хеш объекта values,"
  "используя структуру struct"
  (foldl '(lambda (ofs elem)
	   (let ((name (car elem))
		 (size (cdr elem)))
	     (if (eq name 'str)
		 (when (check-key values (cadr elem))
		   (arr-set-str arr ofs (get-hash values (cadr elem)) (cdr size)))
		 (when (check-key values name)
		   (arr-set-num arr ofs (get-hash values name) size)))
	     (+ ofs (if (eq name 'str) (cdr size) size)))) offs struct))

(defun array-cat (ar1 ar2)
  "Объединить массивы ar1 и ar2"
  (let* ((s1 (array-size ar1))
	 (s2 (array-size ar2))
	 (ar (make-array (+ s1 s2))))
    (for i 0 s1 (seta ar i (aref ar1 i)))
    (for i 0 s2 (seta ar (+ s1 i) (aref ar2 i)))
    ar))

(defun array-seq (ar i1 i2)
  "Получить часть массива ar, начальный индекс i1, конечный индекс(не включая) i2"
  (let* ((size (- i2 i1))
	 (a (make-array size)))
    (for i 0 size (seta a i (aref ar (+ i i1))))
    a))

; тесты
(defun with-struct-test ()
  (let ((s '((str name . 10) ; строковое поле из 10 байт
	     (f2 . 4)))
	(arr #(0 0 0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 1 0 0 0)))
    (with-struct s arr 2
      `(,name ,f2))))

(defun write-struct-test ()
  (let ((s '((str name . 10) ; строковое поле из 10 байт
	     (res . 1)
	     (f2 . 4)))
	(arr #(0 0 0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 0x20 1 0 0 0)))
    (write-struct arr 2 s '((f2 . 10)(name . "abc")(test . 10)))
    arr))
