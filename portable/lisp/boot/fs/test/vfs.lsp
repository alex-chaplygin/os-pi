(defvar *fsinfo-struct* #(82 82 97 65
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                          114 114 65 97
                          255 255 255 255
                          2 0 0 0
                          0 0 0 0 0 0 0 0 0 0 0 0
                          0 0 85 170))

(defvar *bios-parameter-block* #(144 144 144
                                 32 32 32 32 32 32 32 32
                                 0 2
                                 4
                                 3 0
                                 2
                                 0 0
                                 255 255
                                 0
                                 1 0
                                 0 0
                                 0 0
                                 0 0 0 0
                                 0 0 0 0
                                 2 0 0 0
                                 0 0 ;; fat flags
                                 0 0
                                 2 0 0 0
                                 1 0
                                 2 0
                                 0 0 0 0 0 0 0 0 0 0 0 0
                                 0
                                 0
                                 40
                                 0 0 0 0
                                 0 0 0 0 0 0 0 0 0 0 0
                                 32 32 32 32 32 32 32 32
                                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                 85 170))

(defvar *fat-table-sectors* (make-array 1024))
(let ((fat-table-small (make-array 256)))
  (for i 0 256 (seta fat-table-small i +fat-block-free+))
  (seta fat-table-small 2 +fat-block-end+)
  (seta fat-table-small 100 +fat-block-end+)
  (seta fat-table-small 101 +fat-block-end+)
  (seta fat-table-small 102 +fat-block-end+)
  (for i 0 256
       (arr-set-num
        *fat-table-sectors*
        (* i 4)
        (aref fat-table-small i)
        4)))

;;firstfile.txt
(defvar *lfn-entry1* #(65
                       0x66 0 0x69 0 0x72 0 0x73 0 0x74 0
                       0xF
                       0
                       1
                       0x66 0 0x69 0 0x6c 0 0x65 0 0x2e 0 0x74 0
                       0 0
                       0x78 0 0x74 0))

;;first dir
(defvar *lfn-entry2* #(65
                       0x66 0 0x69 0 0x72 0 0x73 0 0x74 0
                       0xF
                       0
                       1
                       0x20 0 0x64 0 0x69 0 0x72 0 0xFF 0xFF 0xFF 0xFF
                       0 0
                       0xFF 0xFF 0xFF 0xFF))


;;karas.txt
(defvar *dir-entry1* #(107 97 114 97 115 32 32 32 116 120 116
                       0
                       0
                       120
                       0xD6 0x63
                       0xD9 0x16
                       0xD9 0x16
                       0 0
                       0xD6 0x63
                       0xD9 0x16
                       100 0
                       96 0 0 0))
;;dir
(defvar *dir-entry2* #(0x64 0x69 0x72 0x20 0x20 0x20 0x20 0x20 0x20 0x20 0x20
                       16
                       0
                       120
                       0xD6 0x63
                       0xD9 0x16
                       0xD9 0x16
                       0 0
                       0xD6 0x63
                       0xD9 0x16
                       101 0
                       96 0 0 0))
;;karas.txt
(defvar *dir-entry3* #(107 97 114 97 115 32 32 32 116 120 116
                       0
                       0
                       120
                       0xD6 0x63
                       0xD9 0x16
                       0xD9 0x16
                       0 0
                       0xD6 0x63
                       0xD9 0x16
                       102 0
                       96 0 0 0))


(setq *disk* 0)

(defun init-fat32-test-fs ()
  (let ((lfn-entry1 *lfn-entry1*)
        (lfn-entry2 *lfn-entry2*)
        (dir-entry1 *dir-entry1*)
        (dir-entry2 *dir-entry2*)
        (dir-entry3 *dir-entry3*)
        (bios-block *bios-parameter-block*)
        (fsinfo-struct *fsinfo-struct*)
        (fat-table-sectors *fat-table-sectors*)
        (test-sector nil))
    (setq *file-system* (make-instance 'FAT32FileSystem))
    (ata-write-sectors *disk* 0 1 bios-block)
    (ata-write-sectors *disk* 1 1 fsinfo-struct)
    (ata-write-sectors *disk* 2 1 bios-block)
    (ata-write-sectors *disk* 3 2 fat-table-sectors)
    (ata-write-sectors *disk* 5 2 fat-table-sectors)
    (fs-init *file-system* *disk* 0 65535)
    (setq test-sector (make-array 1920))
    (for i 0 1920 (seta test-sector i 0))
    (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
                                 test-sector))
    (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
                                 test-sector))
    (block-write 2 test-sector)
    (setq test-sector (make-array 1984))
    (for i 0 1984 (seta test-sector i 0))
    (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry3)
                                 test-sector))
    (block-write 101 test-sector)))






(deftest join-test () "join test"
         "Проверка конкатинации строк с разделителем"
         (assert (join #\. '("a" "b" "c" "d"))
                 "a.b.c.d"))

(deftest parse-path-test () "parse path test"
         "Проверка проеобразования пути"
         (setq *working-path* '("root"))
         (assert (parse-path "/") '("root"))
         (assert (parse-path "") '("working"))
         (assert (parse-path "/karas.txt") '("root" "karas.txt"))
         (assert (parse-path "karas.txt") '("working" "karas.txt"))
         (assert (parse-path "/karas/../karas1.txt") '("root" "karas1.txt"))
         (setq *working-path* '("root" "dir"))
         (assert (parse-path "../karas2.txt") '("root" "karas2.txt"))
         (setq *working-path* '("root" "dir1" "dir2" "dir3" "dir4"))
         ;; (print (parse-path "../../../../../sas")) ;; вызов ошибки
         (assert (parse-path "../../karas3.txt") '("root" "dir1" "dir2" "karas3.txt")))

(deftest load-path-test () "load path test"
         "Проверка загрузки пути"
         (init-fat32-test-fs)
         (assert (load-path "/") *root-directory*)
         (assert (load-path "/firstfile.txt") (caar (get-hash *root-directory* 'dir)))
         ;; (load-path "/firstfile.txt/karas.bin") ;; вызов ошибки
         (assert (load-path "/first dir/firstfile.txt") (caar (get-hash (cadar (get-hash *root-directory* 'dir)) 'dir)))
         (setq *working-path* '("root" "first dir"))
         (setq *working-directory* (cadar (get-hash *root-directory* 'dir)))
         (assert (load-path "firstfile.txt") (caar (get-hash (cadar (get-hash *root-directory* 'dir)) 'dir))))

(deftest get-parent-dir-test () "get parent dir test"
         "Проверка получения родительского каталога"
         (init-fat32-test-fs)
         (load-path "/first dir/firstfile.txt")
         (setq *working-path* '("root" "first dir"))
         (setq *working-directory* (cadar (get-hash *root-directory* 'dir)))
         (assert (get-parent-dir "firstfile.txt") (cadar (get-hash *root-directory* 'dir))))

(deftest list-dir-test () "list dir test"
         "Проверка получия содержимого каталога"
         (init-fat32-test-fs)
         (assert (car (list-dir "/first dir")) "firstfile.txt"))

(deftest create-dir-test () "create dir test"
         "Проверка создания каталога"
         (init-fat32-test-fs)
         (create-dir "/first dir/secondfile")
         (assert (list-dir "/first dir") '("firstfile.txt" "secondfile")))

(deftest remove-dir-test () "remove dir test"
         "Проверка удаления каталога"
         (init-fat32-test-fs)
         (create-dir "/first dir/secondfile")
         (remove-dir "/first dir/secondfile")
         (assert (list-dir "/first dir") '("firstfile.txt")))

(deftest change-dir-test () "change dir test"
         "Проверка перемещения в каталог"
         (init-fat32-test-fs)
         (change-dir "/first dir")
         (assert (cur-dir) "/first dir"))

(deftest create-file-test () "create file test"
         "Проверка создания файла"
         (init-fat32-test-fs)
         (create-file "/first dir/secondfile.txt")
         (assert (second (list-dir "/first dir")) "secondfile.txt"))

(deftest remove-file-test () "remove file test"
         "Проверка удаления файла"
         (init-fat32-test-fs)
         (create-file "/first dir/secondfile.txt")
         (remove-file "/first dir/secondfile.txt")
         (assert (list-dir "/first dir") '("firstfile.txt")))

(deftest open-file-test () "open file test"
         "Проверка открытия файла"
         (init-fat32-test-fs)
         (let ((file (open-file "/first dir/firstfile.txt")))
           (assert (list (FAT32File-name file)
                         (FAT32File-blocks file)
                         (FAT32File-size file))
		   (list "firstfile.txt" '(102) 96))))

(deftest is-directory-test () "is directory test"
         "Проверка на каталог"
         (init-fat32-test-fs)
         (assert (list (is-directory "/first dir")
                       (is-directory "/first dir/firstfile.txt"))
		 (list t nil)))

(deftest rename-test () "rename test"
         "Проверка переименовывания файла"
         (init-fat32-test-fs)
         (rename "/first dir/firstfile.txt" "secondfile.txt")
         (assert (car (list-dir "/first dir")) "secondfile.txt"))

(deftest free-space-test () "free space test"
         "Проверка получения свободного места"
         (init-fat32-test-fs)
         (assert (free-space) (* 250 *block-size*)))

(deftest fstat-test () "fstat test"
         "Проверка получения метаданных"
         (init-fat32-test-fs)
         (let ((test-stat (make-hash)))
           (set-hash test-stat 'name "firstfile.txt")
           (set-hash test-stat 'size 96)
           (set-hash test-stat 'create-date '(25 6 1991))
           (set-hash test-stat 'create-time '(12 30 45))
           (set-hash test-stat 'modify-date '(25 6 1991))
           (set-hash test-stat 'modify-time '(12 30 44))
           (set-hash test-stat 'access-date '(25 6 1991))
           (set-hash test-stat 'access-time nil)
           (set-hash test-stat 'isdir nil)
           (set-hash test-stat 'create-time-ms 200)
           (set-hash test-stat 'flags nil)
           (assert (fstat "/first dir/firstfile.txt") test-stat)))

(deftest read-file-test () "work with file test"
         "Проверка работы с файлом"
         (init-fat32-test-fs)
         (let ((file (open-file "/first dir/firstfile.txt"))
               (test-str "file content")
               (test-buf (make-array 12)))
           (arr-set-str test-buf 0 test-str 12)
           (write-file file test-buf)
           (seek-file file 0 'SET)
           (assert (read-file file 12) test-buf)))

(deftest set-attr-test () "set attributes test"
         "Проверка изменения атрибутов"
         (init-fat32-test-fs)
         (set-attr "/first dir" '(HIDDEN))
         (assert
          (get-hash (fstat "/first dir") 'flags)
          '(HIDDEN DIRECTORY)))
