(deftest get-date-time-entry-test () "get date time from entry test"
         "Проверка получения даты и времени из строки байт"
         (let* ((date-time-arr #(41 6 22 3 15 0 0)))
           (assert (get-date-time-entry date-time-arr) '(1941 6 22 3 15 0))))

(deftest get-attributes-entry-test1 () "get attributes from entry test 1"
         "Проверка получения списка атрибутов из байта"
         (let ((attr-byte 19))
           (assert (get-attributes-entry attr-byte) '(PERMISSIONS-IN-EXT-ATTR DIRECTORY HIDDEN))))
(deftest get-attributes-entry-test2 () "get attributes from entry test 2"
         "Проверка получения списка атрибутов из байта"
         (let ((attr-byte 0))
           (assert (get-attributes-entry attr-byte) ())))
(deftest get-attributes-entry-test3 () "get attributes from entry test 3"
         "Проверка получения списка атрибутов из байта"
         (let ((attr-byte 254))
           (assert (get-attributes-entry attr-byte) '(NOT-LAST-ENTRY PERMISSIONS-IN-EXT-ATTR FORMAT-IN-EXT-ATTR ASSOCIATIVE DIRECTORY))))

(deftest parse-root-dir-entry-test () "parse root dir entry test"
         "Проверка получения хеш-объекта корневого каталога"
         (set-block-size 4)
         (let ((root-dir-entry #(34
                                 0
                                 32 0 0 0
                                 0 0 0 32
                                 216 4 0 0
                                 0 0 4 216
                                 45 5 9 0 43 0 0
                                 2
                                 0
                                 0
                                 0 1
                                 1 0
                                 1
                                 0))
               (test-hash (make-hash)))
           (set-hash test-hash 'name (make-string 1 (code-char 0)))
           (set-hash test-hash 'size 1240)
           (set-hash test-hash 'blocks '(32))
           (set-hash test-hash 'creation-date-time '(1945 5 9 0 43 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (assert (car (funcall (parse-cdfs-dir-entry) (stream-from-arr root-dir-entry nil))) test-hash)
           ))

(deftest parse-cdfs-dir-entry-test1 () "parse dir entry test 1"
         "Проверка получения хеш-объекта файла"
         (set-block-size 4)
         (let ((dir-entry #(46
                            0
                            64 0 0 0
                            0 0 0 64
                            256 4 0 0
                            0 0 4 256
                            41 6 22 3 15 0 0
                            1
                            0
                            0
                            0 1
                            1 0
                            12
                            107 97 114 97 115 49 46 116 120 116 59 49
                            0))
               (test-hash (make-hash)))
           (set-hash test-hash 'name "karas1.txt;1")
           (set-hash test-hash 'size 1280)
           (set-hash test-hash 'blocks '(64))
           (set-hash test-hash 'creation-date-time '(1941 6 22 3 15 0))
           (set-hash test-hash 'attributes '(HIDDEN))
           (set-hash test-hash 'dir nil)
           (assert (car (funcall (parse-cdfs-dir-entry) (stream-from-arr dir-entry nil))) test-hash)
           ))

(deftest parse-cdfs-dir-entry-test2 () "parse dir entry test 2"
         "Проверка получения 2 хеш-объектов: каталога и файла"
         (set-block-size 4)
         (let* ((dir-entry-1 #(42
                               0
                               128 0 0 0
                               0 0 0 128
                               0 4 0 0
                               0 0 4 0
                               41 6 22 3 15 0 0
                               2
                               0
                               0
                               0 1
                               1 0
                               8
                               107 97 114 97 115 49 59 49
                               0))
		(dir-entry-2 #(46
                               0
                               64 0 0 0
                               0 0 0 64
                               256 4 0 0
                               0 0 4 256
                               41 6 22 3 0 0 0
                               0
                               0
                               0
                               0 1
                               1 0
                               12
                               107 97 114 97 115 50 46 116 120 116 59 49
                               0))
		(test-hash1 (make-hash))
		(test-hash2 (make-hash))
		(dirs-stream (stream-from-arr (array-cat dir-entry-1 dir-entry-2) nil))
		(entry1 nil)
		(entry2 nil))
           (set-hash test-hash1 'name "karas1;1")
           (set-hash test-hash1 'size 1024)
           (set-hash test-hash1 'blocks '(128))
           (set-hash test-hash1 'creation-date-time '(1941 6 22 3 15 0))
           (set-hash test-hash1 'attributes '(DIRECTORY))
           (set-hash test-hash1 'dir t)
           (set-hash test-hash2 'name "karas2.txt;1")
           (set-hash test-hash2 'size 1280)
           (set-hash test-hash2 'blocks '(64))
           (set-hash test-hash2 'creation-date-time '(1941 6 22 3 0 0))
           (set-hash test-hash2 'attributes ())
           (set-hash test-hash2 'dir nil)
           (setq entry1 (funcall (parse-cdfs-dir-entry) dirs-stream))
           (setq dirs-stream (cdr entry1))
           (setq entry1 (car entry1))
           (setq entry2 (car (funcall (parse-cdfs-dir-entry) dirs-stream)))
           (assert (append entry1 entry2) (append test-hash1 test-hash2))
           ))

(deftest parse-cdfs-dir-entry-test3 () "parse dir entry test 3"
         "Проверка получения 2 хеш-объектов: каталога и файла"
         (set-block-size 4)
         (let* ((dir-entry-1 #(41
                               0
                               128 0 0 0
                               0 0 0 128
                               0 4 0 0
                               0 0 4 0
                               41 6 22 3 15 0 0
                               2
                               0
                               0
                               0 1
                               1 0
                               7
                               107 97 114 97 115 59 49))
		(dir-entry-2 #(46
                               0
                               64 0 0 0
                               0 0 0 64
                               256 4 0 0
                               0 0 4 256
                               41 6 22 3 0 0 0
                               0
                               0
                               0
                               0 1
                               1 0
                               12
                               107 97 114 97 115 50 46 116 120 116 59 49
                               0))
		(test-hash1 (make-hash))
		(test-hash2 (make-hash))
		(dirs-stream (stream-from-arr (array-cat dir-entry-1 dir-entry-2) nil))
		(entry1 nil)
		(entry2 nil))
           (set-hash test-hash1 'name "karas;1")
           (set-hash test-hash1 'size 1024)
           (set-hash test-hash1 'blocks '(128))
           (set-hash test-hash1 'creation-date-time '(1941 6 22 3 15 0))
           (set-hash test-hash1 'attributes '(DIRECTORY))
           (set-hash test-hash1 'dir t)
           (set-hash test-hash2 'name "karas2.txt;1")
           (set-hash test-hash2 'size 1280)
           (set-hash test-hash2 'blocks '(64))
           (set-hash test-hash2 'creation-date-time '(1941 6 22 3 0 0))
           (set-hash test-hash2 'attributes ())
           (set-hash test-hash2 'dir nil)
           (setq entry1 (funcall (parse-cdfs-dir-entry) dirs-stream))
           (setq dirs-stream (cdr entry1))
           (setq entry1 (car entry1))
           (setq entry2 (car (funcall (parse-cdfs-dir-entry) dirs-stream)))
           (assert (append entry1 entry2) (append test-hash1 test-hash2))
           ))

(deftest parse-descriptor-test1 () "parse boot record descriptor test"
         "Проверка парсинга дескрипторов типа Boot Record"
         (let ((descriptor (make-array 2048))
               (cdfsfs (make-instance 'CDFSFileSystem)))
           (seta descriptor 0 0)
           (for i 1 6 (seta descriptor i 55))
           (seta descriptor 6 1)
           (for i 7 39 (seta descriptor i 66))
           (for i 39 71 (seta descriptor i 77))
           (for i 71 2048 (seta descriptor i 0))
           (assert (car (funcall (parse-descriptor cdfsfs) (stream-from-arr descriptor nil))) t)))

(deftest parse-descriptor-test2 () "parse descriptor set terminator test"
         "Проверка парсинга дескрипторов типа Volume Descriptor Set Terminator"
         (let ((descriptor (make-array 2048))
               (cdfsfs (make-instance 'CDFSFileSystem)))
           (seta descriptor 0 255)
           (for i 1 6 (seta descriptor i 55))
           (seta descriptor 6 1)
           (for i 7 2048 (seta descriptor i 0))
           (assert (car (funcall (parse-descriptor cdfsfs) (stream-from-arr descriptor nil))) nil)))

(setq primary-descriptor
      (progn
	(let ((primary-descriptor-add (make-array 1165))
              (primary-descriptor1
               #(1
		 2 2 2 2 2
		 1
		 0
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 0 0 0 0 0 0 0 0
		 0 0 0 1 ;; volume-size
		 1 0 0 0
		 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		 0 0 0 0
		 0 0 0 0
		 0 8 ;; block-size
		 8 0
		 0 4 0 0 ;; path-table-size
		 4 0 0 0
		 128 0 0 0 ;; path-table-sector
		 138 0 0 0
		 0 0 0 158
		 0 0 0 168
		 34 ;; root-dir-entry start
		 0
		 32 0 0 0
		 0 0 0 32
		 216 4 0 0
		 0 0 4 216
		 45 5 9 0 43 0 0
		 2
		 0
		 0
		 0 1
		 1 0
		 1
		 0 ;; root-dir-entry end
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
		 49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
		 49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
		 49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
		 49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
		 1
		 0)))
	  (for i 0 1165 (seta primary-descriptor-add i 0))
	  (array-cat primary-descriptor1 primary-descriptor-add))))

(deftest parse-descriptor-test3 () "parse primary volume descriptor test"
         "Проверка парсинга дескрипторов типа Primary Volume Descriptor"
         (set-block-size 4)
         (let* ((test-hash (make-hash))
		(cdfsfs (make-instance 'CDFSFileSystem))
		(parse-res (car (funcall (parse-descriptor cdfsfs) (stream-from-arr primary-descriptor nil)))))
           (set-hash test-hash 'name "root")
           (set-hash test-hash 'size 1240)
           (set-hash test-hash 'blocks '(32))
           (set-hash test-hash 'creation-date-time '(1945 5 9 0 43 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (assert
            (list parse-res *block-sector-offset* *block-size* (CDFSFileSystem-fs-block-count cdfsfs) (CDFSFileSystem-path-table-size cdfsfs) (CDFSFileSystem-path-table-sector cdfsfs) (CDFSFileSystem-root-entry cdfsfs))
            (list t 0 2048 16777216 1024 128 test-hash))))

(deftest fs-init-test () "fs init test"
         "Проверка загрузки файловой системы ISO9660 с диска"
         (let ((test-hash (make-hash))
               (cdfsfs (make-instance 'CDFSFileSystem))
               (test-sectors nil))
           (set-hash test-hash 'name "root")
           (set-hash test-hash 'size 1240)
           (set-hash test-hash 'blocks '(32))
           (set-hash test-hash 'creation-date-time '(1945 5 9 0 43 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (setq test-sectors (make-array 32768))
           (for i 0 32768 (seta test-sectors i 0))
           (ata-write-sectors 0 0 64 test-sectors)
           (setq test-sectors (make-array 2048))
           (seta test-sectors 0 0)
           (for i 1 6 (seta test-sectors i 55))
           (seta test-sectors 6 1)
           (for i 7 39 (seta test-sectors i 66))
           (for i 39 71 (seta test-sectors i 77))
           (for i 71 2048 (seta test-sectors i 0))
           (ata-write-sectors 0 64 4 test-sectors)
           (ata-write-sectors 0 68 4 primary-descriptor)
           (seta test-sectors 0 255)
           (for i 1 6 (seta test-sectors i 55))
           (seta test-sectors 6 1)
           (for i 7 2048 (seta test-sectors i 0))
           (ata-write-sectors 0 72 4 test-sectors)
           (fs-init cdfsfs 0 0 1000)
           (assert
            (list *block-sector-offset* *block-size* (CDFSFileSystem-fs-block-count cdfsfs) (CDFSFileSystem-path-table-size cdfsfs) (CDFSFileSystem-path-table-sector cdfsfs) (CDFSFileSystem-root-entry cdfsfs))
            (list 0 2048 16777216 1024 128 test-hash))
           ))

(deftest load-dir-test () "load dir test"
         "Проверка раскрытия каталога"
         (let ((test-hash (make-hash))
               (cdfsfs (make-instance 'CDFSFileSystem))
               (test-sectors nil)
               (entry1 #(
			 ;; dir-entry start
			 50 ;; dir-entry-len
			 0
			 22 0 0 0 ;; block-num
			 0 0 0 22
			 21 0 0 0 ;; size
			 0 0 0 21
			 125 1 7 12 0 0 0 ;; creation-date-time
			 0 ;; attributes
			 0
			 0
			 0 1
			 1 0
			 16 ;; name-len
			 102 105 114 115 116 45 102 105 108 101 46 116 120 116 59 49 ;; name first-file.txt;1
			 0 ;; padding
			 ;;dir-entry end
			 ))
               (entry2 #(
			 ;; dir-entry start
			 44 ;; dir-entry-len
			 0
			 23 0 0 0 ;; block-num
			 0 0 0 22
			 50 0 0 0 ;; size
			 0 0 0 50
			 125 4 7 22 30 0 0 ;; creation-date-time
			 2 ;; attributes directory
			 0
			 0
			 0 1
			 1 0
			 11 ;; name-len
			 102 105 114 115 116 45 100 105 114 59 49 ;; name first-dir;1
			 ;;dir-entry end
			 )))
           (set-block-size 1)
           (set-hash test-hash 'name "root")
           (set-hash test-hash 'size 94)
           (set-hash test-hash 'blocks '(50))
           (set-hash test-hash 'creation-date-time '(1945 5 9 0 43 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (setq test-sectors (make-array 418))
           (for i 0 34 (seta test-sectors i 20))
           (for i 34 418 (seta test-sectors i 0))
           (block-write 50 (array-cat (array-cat entry1 entry2) test-sectors))
           (load-dir cdfsfs test-hash)
           (assert (get-hash test-hash 'dir) '((HASH (NAME . "first-file.txt;1") (SIZE . 21) (BLOCKS 22) (CREATION-DATE-TIME 2025 1 7 12 0 0) (ATTRIBUTES) (DIR)) (HASH (NAME . "first-dir;1") (SIZE . 50) (BLOCKS 23) (CREATION-DATE-TIME 2025 4 7 22 30 0) (ATTRIBUTES DIRECTORY) (DIR . T))))))

(deftest fstat-test () "fstat test"
         "Проверка получения метаданных файла и каталога"
         (let ((test-hash1 ())
               (test-hash2 ())
               (cdfsfs (make-instance 'CDFSFileSystem)))
           (setq test-hash1 '(HASH (NAME . "first-file.txt;1") (SIZE . 21) (BLOCKS 22) (CREATION-DATE-TIME 2025 1 7 12 0 0) (ATTRIBUTES) (DIR))
                 test-hash2 '(HASH (NAME . "first-dir;1") (SIZE . 50) (BLOCKS 23) (CREATION-DATE-TIME 2025 4 7 22 30 0) (ATTRIBUTES DIRECTORY) (DIR . T)))
           (assert (list (fstat cdfsfs test-hash1) (fstat cdfsfs test-hash2))
                   (list '(HASH (NAME . "first-file.txt;1") (SIZE . 21) (CREATE-DATE 7 1 2025) (CREATE-TIME 12 0 0) (MODIFY-DATE) (MODIFY-TIME) (ACCESS-DATE) (ACCESS-TIME) (ISDIR) (FLAGS))
                         '(HASH (NAME . "first-dir;1") (SIZE . 0) (CREATE-DATE 7 4 2025) (CREATE-TIME 22 30 0) (MODIFY-DATE) (MODIFY-TIME) (ACCESS-DATE) (ACCESS-TIME) (ISDIR . T) (FLAGS DIRECTORY))))))

(deftest open-file-test () "open file test"
         "Проверка открытия файла"
         (let ((test-hash ())
               (cdfsfs (make-instance 'CDFSFileSystem))
               (file-object nil))
           (setq test-hash '(HASH (NAME . "first-file.txt;1") (SIZE . 21) (BLOCKS 22) (CREATION-DATE-TIME 2025 1 7 12 0 0) (ATTRIBUTES) (DIR)))
           (setq file-object (open-file cdfsfs test-hash))
           (assert
            (list
             (CDFSFile-name file-object)
             (CDFSFile-size file-object)
             (CDFSFile-position file-object)
             (CDFSFile-blocks file-object)
             (CDFSFile-dir file-object))
            (list
             "first-file.txt;1"
             21
             '(0 . 0)
             '(22)
             nil))))

(deftest read-file-cdfs-test () "read file test"
         "Проверка на чтении файла файла CDFS"
         (let ((test-sector (make-array 512))
               (file (make-CDFSFile "test" 512 '(0 . 0) '(10) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 10 test-sector)
           (setq file-buf (read-file file 512))
           (assert (cons file-buf (File-position file))
		   (cons test-sector '(1 . 0)))))
