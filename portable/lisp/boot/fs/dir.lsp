; работа с каталогами FAT32
(defconst +dir-end+ 0) ; свободная запись
(defconst +dir-del+ 0xe5) ; удаленная запись
(defconst +entry-size+ 32) ; размер записи каталога
; атрибуты
(defconst +long-name+ 0xf)
(defconst +directory+ 0x10)
;(make-bit-flags +read-only+ +hidden+ +system+ +volume-id+ +directory+ +archive+)
(defvar directory-entry ; базовая запись каталога
  '((str dname . 11) ; имя + расширение
    (attributes . 1) ; атрибуты
    (reserved . 1)
    (ctime-mil . 1) ;  Сотые доли секунды времени создания
    (ctime . 2) ;  Часы/минуты/секунды времени создания (5 бит/6 бит/ 5 бит)
    (cdate . 2) ;  Дата создания (Год 7 бит / Месяц 4 бита / День 5 бит)
    (adate . 2) ;  Дата последнего обращения
    (block-hi . 2) ;  Старшие 2 байта ссылки на первый блок
    (wtime . 2) ; Часы, минуты, секунды времени модификации
    (wdate . 2) ;  Дата модификации
    (block-low . 2) ;  Младшие 2 байта ссылки на первый кластер
    (size . 4))) ;  Размер файла

(defun make-dir (block-num)
  "Создать каталог из данных блока block-num"
  (let ((block (block-read block-num))) ; читаем блок
    (make-dir* block block-num 0)))

(defun make-dir* (block block-num offset)
  "Создаем список из файлов блока block начиная со смещения offset"
  (let ((ch (aref block offset)))
    (case ch
      (+dir-end+ nil)
      (+dir-del+ (make-dir* block block-num (+ offset +entry-size+)))
      (otherwise
       (if (= (aref block (+ offset 11)) +long-name+)  ; пропускаем длинные имена
	   (make-dir* block block-num (+ offset +entry-size+))
	   (with-struct directory-entry block offset
	     (cons
	      (make-fat32file (fat-file-name dname) size 0 nil nil dname
			      block-hi block-low block-num offset attributes
			      (+ block-low (<< block-hi 16))
			      ctime cdate adate wdate wtime)
	  (make-dir* block block-num (+ offset +entry-size+)))))))))

(defun fat-file-name (name)
  "Преобразует имя из записи каталога 8+3 в имя и расширение"
  (let* ((fname (subseq name 0 8))
	 (ext (subseq name 8 11))
	 (fspace (search " " fname))
	 (extspace (search " " ext)))
    (when (not (null fspace)) (setq fname (subseq fname 0 fspace)))
    (when (not (null extspace)) (setq ext (subseq ext 0 extspace)))
    (concat fname (if (= ext "") "" ".") ext)))

(defun fat-dname (name)
  "Сформировать в формате 8+3 имя для записи каталога"
  (let ((dot (search "." name)))
    (if (null dot) (concat name (str-repl (- 11 (string-size name)) " "))
	(let ((nam (subseq name 0 dot))
	      (ext (subseq name (+ 1 dot) (string-size name))))
	  (concat nam (str-repl (- 8 dot) " ")
		  ext (str-repl (- 3 (string-size ext)) " "))))))
		      
(defun update-dir-entry (file)
  "Обновить запись каталога для файла file"
  (let* ((num (slot file 'dir-block))
	 (bl (block-read num)))
    (let ((sb (slot file 'start-block)))
      (setf (slot file 'block-hi) (>> sb 8))
      (setf (slot file 'block-low) (& sb 0xff)))
    (write-struct bl (slot file 'dir-offset) directory-entry file)
    (block-write num bl)))
	
(defun get-free-dir-entry (blocks)
  "Найти первую свободную запись в каталоге из блоков blocks"
  "Возвращает (номер блока.смещение)"
  (if (null blocks) nil
      (let* ((bl (block-read (car blocks)))
	     (num (get-free-dir-entry* bl 0)))
	(if (null num) (get-free-dir-entry (cdr blocks)) (cons (car blocks) num)))))
(defun get-free-dir-entry* (block pos)
  "pos - смещение внутри блока block"
  (if (= pos *block-size*) nil
      (let ((ch (aref block pos)))
	(case ch
	  (+dir-del+ (if (= (aref block (+ pos 11)) +long-name+)
			 (get-free-dir-entry* block (+ pos 32)) pos))
	  (+dir-end+ pos)
	  (otherwise (get-free-dir-entry* block (+ pos 32)))))))

(defun create-file-entry(dir name attr)
  "Создать запись каталога в объекте dir с именем name, атрибуты attr"
  (unless (null (check-key dir name)) (error "Path exists"))
  (let* ((dname (fat-dname name))
	 (blocks (get-fat-chain (slot (cdar dir) 'dir-block)))
	 (pos (get-free-dir-entry blocks)))
    (when (null pos) (error "No free space to create dir entry"))
    (let* ((num (car pos))
	   (bl (block-read num))
	   (ofs (cdr pos))
	   (new-bl (fat-get-free-block)))
      (let ((file (make-fat32file name 0 0 (fat-append-chain nil new-bl) nil dname (>> new-bl 8) (& new-bl 0xFF) num ofs attr new-bl 0 0 0 0 0)))
	(write-struct bl ofs directory-entry file)
	(array-seq bl ofs (+ ofs 32))
	(block-write num bl)
	(set-hash dir name file)))))

(defun create-special-entries (dir)
  "Создать специальные файлы . и .. в объекте-каталоге dir и очистить остальные записи"
  (let* ((num (slot dir 'start-block))
	 (block (block-read num))
	 (file-cur (make-fat32file "." 0 0 nil nil ".          " (>> num 8) (& num 0xff) (slot dir 'dir-block) 0 +directory+ num 0 0 0 0 0))
	 (file-par (make-fat32file ".." 0 0 nil nil "..         " 0 0 0 32 +directory+ num 0 0 0 0 0)))
    (set-hash dir "." file-cur)
    (set-hash dir ".." file-par)
    (write-struct block 0 directory-entry file-cur)
    (write-struct block 32 directory-entry file-par)
    (for i 64 *block-size* (seta block i 0))
    (block-write num block)))

(defun delete-file-entry (file)
  "Удалить запись для объекта-файла file"
  (let* ((num (slot file 'dir-block))
	 (bl (block-read num))
	 (ofs (slot file 'dir-offset)))
    (seta bl ofs +dir-del+)
    (block-write num bl)))
