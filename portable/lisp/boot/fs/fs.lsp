(defvar *file-system*) ; глобальный объект файловой системы
(defvar *root-directory*) ; дерево корневого каталога
(defvar *working-directory*) ; дерево рабочего каталога

; класс абстрактной файловой системы
(defclass FileSystem () ())
; класс абстрактного файла
(defclass File ()
  (name ;    имя
   size ;    размер файла	   
   position ; позиция чтения/записи
   blocks ;   список номеров блоков, например (14 15 16)
   dir)) ;      для каталогов - дерево

(defun load-path (path)
  "Загрузка дерева каталога по абслоютному или относительному пути path"
  (if (= path "/") *root-directory*
      (let ((list (split "/" path)))
	(if (= (car list) "") (load-path* (cdr list) *root-directory*)
	    (load-path* list *working-directory*)))))

(defun load-path* (list dir)
  "Загрузка списка каталогов list относительно каталога dir"
  (if (null list) dir
      (let ((name (car list))) ; имя каталога
	(if (not (check-key dir name)) nil ; если нет такого имени в каталоге
	    (let ((d (get-hash dir name))) ;объект файл/каталог
	      (if (not (is-directory d)) nil ; ошибка - не каталог
		  (progn
		    (when (null (slot d 'blocks))
		      (setf (slot d 'blocks) ; загружаем список блоков если нужно
			  (get-blocks d)))
		    (when (null (slot d 'dir)) ; загружаем каталог, если его нет
		      (setf (slot d 'dir)
			    (load-dir *file-system* (slot d 'blocks))))
		    (load-path* (cdr list) (slot d 'dir)))))))))

(defmacro listdir (path)
  "Просмотр содержимого папки по пути path"
  `(listdir* *file-system* ,path))
(defmethod listdir*((self FileSystem) path)
  (let ((d (load-path path)))
    (if (null d) '(error "Invalid path")
	(map '(lambda (f)
	       (if (is-directory (cdr f)) (list 'dir (car f)) (car f))) d))))

(defmacro chdir (path)
  "Смена рабочего каталога для относительных путей"
  `(chdir* *file-system* ,path))
(defmethod chdir*((self FileSystem) path)
  (let ((d (load-path path)))
    (if (null d) '(error "Invalid path")
	(setq *working-directory* d))))

(defmacro fstat (path)
  "Получение информации о файле/каталоге"
  `(fstat* *file-system* ,path))
(defmethod fstat*((self FileSystem) path)
  (let* ((p (search-back "/" path))
	 (dir-path (if (null p) "" (subseq path 0 p)))
	 (file-name (if (null p) path (subseq path (+ p 1) (string-size path))))
	 (d (load-path dir-path)))
    (if (null d) '(error "Invalid path")
	(if (not (check-key d file-name)) '(error "File not found")
	    (get-hash d file-name)))))

(defmacro fopen (path)
  "Открытие файла, возвращает объект-файл"
  `(fopen* *file-system* ,path))
(defmethod fopen*((self FileSystem) path)
  (let ((f (clone (fstat* self path))))
    (setf (slot f 'position) 0)
    f))

(defmethod fclose ((self File))
  "Закрытие файла"
  nil)

(defmethod fread ((f File) size)
  "Чтение из файла file количество байт size"
  (let ((p (slot f 'position)))
    (if (>= p (slot f 'size)) nil
	(progn
	  (when (null (slot f 'blocks)) (setf (slot f 'blocks) (get-blocks f)))
	  (let* ((pos (get-blocks-pos (slot f 'blocks) p))
		 (bl (block-read (car pos)))
		 (buf (make-array size)))
	    (when (>= (+ (cdr pos) size) *block-size*)
	      (setq size (- *block-size* (cdr pos))))
	    (for i 0 size (seta buf i (aref bl (+ i (cdr pos)))))
	    (setf (slot f 'position) (+ p size))
	    buf)))))
    
