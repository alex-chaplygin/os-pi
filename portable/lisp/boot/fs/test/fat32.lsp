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
  (seta fat-table-small 3 5)
  (seta fat-table-small 5 16)
  (seta fat-table-small 16 150)
  (seta fat-table-small 150 151)
  (seta fat-table-small 151 +fat-block-end+)

  (seta fat-table-small 200 152)
  (seta fat-table-small 152 127)
  (seta fat-table-small 127 160)
  (seta fat-table-small 160 9)
  (seta fat-table-small 9 100)
  (seta fat-table-small 100 +fat-block-end+)
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
                       200 0
                       0xD6 0x63
                       0xD9 0x16
                       0 0
                       96 0 0 0))
;;dir
(defvar *dir-entry2* #(0x64 0x69 0x72 0x20 0x20 0x20 0x20 0x20 0x20 0x20 0x20
                       16
                       0
                       120
                       0xD6 0x63
                       0xD9 0x16
                       0xD9 0x16
                       200 0
                       0xD6 0x63
                       0xD9 0x16
                       0 0
                       96 0 0 0))


(setq *disk* 0)

					; (deftest parse-bios-parameter-block-test () "parse bios parameter block test"
					;          "Проверка парсера блока параметров биос"
					;          (let ((bios-block *bios-parameter-block*)
					;                (root-entry (make-hash)))
					;            (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash root-entry 'name 'root)
					;            (set-hash root-entry 'size -1)
					;            (set-hash root-entry 'first-block 2)
					;            (set-hash root-entry 'creation-date-time nil)
					;            (set-hash root-entry 'modify-date-time nil)
					;            (set-hash root-entry 'access-date nil)
					;            (set-hash root-entry 'attributes '(DIRECTORY))
					;            (set-hash root-entry 'parent-first-block nil)
					;            (set-hash root-entry 'dir t)
					;            (funcall (parse-bios-parameter-block) (stream-from-arr bios-block nil))
					;            (assert
					;             (list (FAT32FileSystem-fat-start-sectors *file-system*)
					;                   (FAT32FileSystem-fat-sectors *file-system*)
					;                   (FAT32FileSystem-fat-active-num *file-system*)
					;                   (FAT32FileSystem-fat-copying *file-system*)
					;                   (FAT32FileSystem-fsinfo-sector *file-system*)
					;                   (FAT32FileSystem-volume-size *file-system*)
					;                   (FAT32FileSystem-root-entry *file-system*))
					;             (list #(3 5)
					;                   2
					;                   0
					;                   t
					;                   1
					;                   65535
					;                   root-entry))))

					; (deftest parse-fsinfo-test () "parse fsinfo test"
					;          "Проверка парсера структуры FSInfo"
					;          (let ((fsinfo-struct *fsinfo-struct*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (funcall (parse-fsinfo) (stream-from-arr fsinfo-struct nil))
					;            (assert
					;             (list (FAT32FileSystem-free-blocks-count *file-system*)
					;                   (FAT32FileSystem-free-block-num *file-system*))
					;             (list 0xffffffff
					;                   2))))

					; (deftest fat32-get-date-time-test-1 () "get creation date-time test"
					;          "Проверка получения списка даты и времени для времени создания"
					;          (assert (fat32-get-date-time #(0xD9 0x16) #(0xD6 0x63) #(120))
					; 		 (list 1991
					;                        6
					;                        25
					;                        12
					;                        30
					;                        45
					;                        200)))

					; (deftest fat32-get-date-time-test-2 () "get modify date-time test"
					;          "Проверка получения списка даты и времени для времени последней модификации"
					;          (assert (fat32-get-date-time #(0xD9 0x16) #(0xD6 0x63))
					; 		 (list 1991
					;                        6
					;                        25
					;                        12
					;                        30
					;                        44)))

					; (deftest fat32-get-date-time-test-3 () "get access date-time test"
					;          "Проверка получения списка даты и времени для времени последнего доступа"
					;          (assert (fat32-get-date-time #(0xD9 0x16) nil)
					; 		 (list 1991 6 25)))

					; (deftest fat32-get-attributes-test-1 () "get attributes test 1"
					;          "Проверка получения списка аттрибутов из байта"
					;          (assert (fat32-get-attributes #(38))
					; 		 (list 'ARCHIVE 'SYSTEM 'HIDDEN)))

					; (deftest fat32-get-attributes-test-2 () "get attributes test 2"
					;          "Проверка получения списка аттрибутов из байта"
					;          (assert (fat32-get-attributes #(19))
					; 		 (list 'DIRECTORY 'HIDDEN 'READ-ONLY)))

					; (deftest fat-elem-pos-test () "fat elem pos test"
					;          "Проверка получения позиции для элемента таблицы FAT"
					;          (assert (fat-elem-pos 90)
					;                  '(0 . 360)))

					; (deftest get-fat-chain-test () "get fat chain test"
					;          "Проверка получения списка цепочек таблицы FAT"
					;          (let ((fat-table-sectors *fat-table-sectors*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (FAT32FileSystem-set-fat-active-num *file-system* 0)
					;            (FAT32FileSystem-set-fat-sectors *file-system* 2)
					;            (FAT32FileSystem-set-fat-start-sectors *file-system* #(5))
					;            (FAT32FileSystem-set-fat-table *file-system* (make-hash))
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (assert (list (get-fat-chain 2)
					;                          (get-fat-chain 3)
					;                          (get-fat-chain 200))
					; 		   (list '(2)
					; 			 '(3 5 16 150 151)
					; 			 '(200 152 127 160 9 100)))
					;            ))

					; (deftest parse-lfn-entry-test1 () "parse lfn test 1"
					;          "Проверка парсера структуры записи длинного имени"
					;          (let ((lfn-entry *lfn-entry1*))
					;            (assert (car (funcall (parse-lfn-entry) (stream-from-arr lfn-entry nil)))
					; 		   "firstfile.txt")))

					; (deftest parse-lfn-entry-test2 () "parse lfn test 2"
					;          "Проверка парсера структуры записи длинного имени"
					;          (let ((lfn-entry *lfn-entry2*))
					;            (assert (car (funcall (parse-lfn-entry) (stream-from-arr lfn-entry nil)))
					; 		   "first dir")))

					; (deftest parse-fat32-dir-entry-test () "parse dir entry test"
					;          "Проверка парсера структуры записи в каталоге"
					;          (let ((dir-entry *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (assert (car (funcall (parse-fat32-dir-entry) (stream-from-arr dir-entry nil)))
					; 		   test-hash)))

					; (deftest fs-init-test () "fs-init test"
					;          "Проверка загруки файловой системы с диска"
					;          (let ((bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (root-entry (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash root-entry 'name 'root)
					;            (set-hash root-entry 'size 0)
					;            (set-hash root-entry 'first-block 2)
					;            (set-hash root-entry 'creation-date-time nil)
					;            (set-hash root-entry 'modify-date-time nil)
					;            (set-hash root-entry 'access-date nil)
					;            (set-hash root-entry 'attributes '(DIRECTORY))
					;            (set-hash root-entry 'parent-first-block nil)
					;            (set-hash root-entry 'dir t)
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (assert
					;             (list (FAT32FileSystem-fat-start-sectors *file-system*)
					;                   (FAT32FileSystem-fat-sectors *file-system*)
					;                   (FAT32FileSystem-fat-active-num *file-system*)
					;                   (FAT32FileSystem-fat-copying *file-system*)
					;                   (FAT32FileSystem-fsinfo-sector *file-system*)
					;                   (FAT32FileSystem-volume-size *file-system*)
					;                   (FAT32FileSystem-root-entry *file-system*)
					;                   (FAT32FileSystem-free-blocks-count *file-system*)
					;                   (FAT32FileSystem-free-block-num *file-system*))
					;             (list #(3 5)
					;                   2
					;                   0
					;                   t
					;                   1
					;                   65535
					;                   root-entry
					;                   0xffffffff
					;                   2))))

					; (deftest load-dir-test1 () "load dir test 1"
					;          "Проверка раскрытия и сохранения содержимого корневого каталога"
					;          (let ((lfn-entry *lfn-entry2*)
					;                (dir-entry *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil)
					;                (test-hash (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1984))
					;            (for i 0 1984 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry dir-entry)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (assert (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)
					;                    (list (list test-hash)))))

					; (deftest load-dir-test2 () "load dir test 2"
					;          "Проверка раскрытия и сохранения содержимого корневого каталога c несколькими записями длинного имени"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil)
					;                (test-hash (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "firstfile.txtfirst dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1952))
					;            (for i 0 1952 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat (array-cat lfn-entry2 lfn-entry1) dir-entry) test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (assert (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)
					;                    (list (list test-hash)))))

					; (deftest fstat-test () "fstat test"
					;          "Проверка получения метаданных каталога"
					;          (let ((test-hash (make-hash))
					;                (test-stat (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dirfirstfile.txt")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'dir t)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-stat 'name "first dirfirstfile.txt")
					;            (set-hash test-stat 'size 0)
					;            (set-hash test-stat 'create-date '(25 6 1991))
					;            (set-hash test-stat 'create-time '(12 30 45))
					;            (set-hash test-stat 'modify-date '(25 6 1991))
					;            (set-hash test-stat 'modify-time '(12 30 44))
					;            (set-hash test-stat 'access-date '(25 6 1991))
					;            (set-hash test-stat 'access-time nil)
					;            (set-hash test-stat 'isdir t)
					;            (set-hash test-stat 'create-time-ms 200)
					;            (set-hash test-stat 'flags '(DIRECTORY))
					;            (assert (fstat *file-system* test-hash) test-stat)))

					; (deftest fat32-generate-date-time-test1 () "generate creation date-time bytes test"
					;          "Проверка генерации массива байт для даты и времени создания"
					;          (let* ((date-time '(1991 12 26 9 40 1 100))
					;                 (date-time-bytes (fat32-generate-date-time date-time)))
					;            (assert (fat32-get-date-time (array-seq date-time-bytes 3 5)
					;                                         (array-seq date-time-bytes 1 3)
					;                                         (array-seq date-time-bytes 0 1))
					; 		   date-time)))

					; (deftest fat32-generate-date-time-test2 () "generate modify date-time bytes test"
					;          "Проверка генерации массива байт для даты и времени последней модификации"
					;          (let* ((date-time '(1991 12 26 9 40 2))
					;                 (date-time-bytes (fat32-generate-date-time date-time)))
					;            (assert (fat32-get-date-time (array-seq date-time-bytes 2 4)
					;                                         (array-seq date-time-bytes 0 2))
					; 		   date-time)))

					; (deftest fat32-generate-date-time-test3 () "generate access date-time bytes test"
					;          "Проверка генерации массива байт для даты последнего доступа"
					;          (let* ((date-time '(1991 12 26))
					;                 (date-time-bytes (fat32-generate-date-time date-time)))
					;            (assert (fat32-get-date-time (array-seq date-time-bytes 0 2)
					;                                         nil)
					;                date-time)))

					; (deftest fat32-generate-attribute-byte-test1 () "generate attribute byte test 1"
					;          "Проверка генерации байта из списка атрибутов"
					;          (assert (fat32-generate-attribute-byte (list 'ARCHIVE 'SYSTEM 'HIDDEN))
					;                  38))

					; (deftest fat32-generate-attribute-byte-test2 () "generate attribute byte test 2"
					;          "Проверка генерации байта из списка атрибутов"
					;          (assert (fat32-generate-attribute-byte (list 'DIRECTORY 'HIDDEN 'READ-ONLY))
					;                  19))

					; (deftest fat32-calc-lfn-count-test1 () "calculate lfn count test 1"
					;          "Подсчитать количество записей длинного имени"
					;          (assert (fat32-calc-lfn-count "under13")
					;                  1))

					; (deftest fat32-calc-lfn-count-test2 () "calculate lfn count test 2"
					;          "Подсчитать количество записей длинного имени"
					;          (assert (fat32-calc-lfn-count "more-than-13-symbols")
					;                  2))

					; (deftest fat32-calc-lfn-count-test3 () "calculate lfn count test 3"
					;          "Подсчитать количество записей длинного имени"
					;          (assert (fat32-calc-lfn-count (make-string 129 #\a))
					;                  10))

					; (deftest fat32-generate-checksum-test () "generate checksum test"
					;          "Проверка подсчёта контрольной суммы короткого имени"
					;          (assert (fat32-generate-checksum "        .   ")
					;                  63))

					; (deftest fat32-generate-checksum-test () "generate checksum test"
					;          "Проверка подсчёта контрольной суммы короткого имени"
					;          (assert (fat32-generate-checksum "karas.txt")
					;                  215))

					; (deftest fat32-create-lfn-entry-test1 () "create lfn entry test 1"
					;          "Проверка создания записи длинного имени"
					;          (assert (fat32-create-lfn-entry "firstfile.txt" 1 1)
					; 		 *lfn-entry1*))

					; (deftest fat32-create-lfn-entry-test2 () "create lfn entry test 2"
					;          "Проверка создания записи длинного имени"
					;          (assert (fat32-create-lfn-entry "first dir" 1 1)
					; 		 *lfn-entry2*))

					; (deftest fat32-create-lfn-entries-test () "create lfn entries test"
					;          "Проверка создания записей длинного имени для записи в каталоге"
					;          (let ((lfn-entries (array-cat *lfn-entry2* *lfn-entry1*))
					;                (test-hash (make-hash)))
					;            (set-hash test-hash 'name "firstfile.txtfirst dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'dir t)
					;            (set-hash test-hash 'short-name (concat (concat (make-string 8 (code-char 1)) ".") (make-string 3 (code-char 1))))
					;            (seta lfn-entries 0 66)
					;            (assert (fat32-create-lfn-entries test-hash)
					; 		   lfn-entries)))

					; (deftest fat32-create-dir-entry-test () "create dir entry test"
					;          "Проверка создания записи в каталоге"
					;          (let ((dir-entry *dir-entry2*)
					;                (test-hash (make-hash))
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'dir t)
					;            (set-hash test-hash 'short-name "dir")

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)

					;            (assert (fat32-create-dir-entry test-hash)
					; 		   dir-entry)))

					; (deftest fat32-create-lfn-dir-entries-test () "create both lfn and dir entry test"
					;          "Проверка создания записей длинного имени и записи в каталоге"
					;          (let ((lfn-entries (array-cat *lfn-entry2* *lfn-entry1*))
					;                (dir-entry *dir-entry2*)
					;                (test-hash (make-hash))
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "firstfile.txtfirst dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'dir t)
					;            (set-hash test-hash 'short-name "dir")
					;            (seta lfn-entries 0 66)
					;            (seta lfn-entries 13 64)
					;            (seta lfn-entries 45 64)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)

					;            (assert (fat32-create-lfn-dir-entries test-hash)
					; 		   (array-cat lfn-entries dir-entry))))

					; (deftest fat32-find-entry-pos-test () "find entry pos test"
					;          "Проверка поиска позиции для записи в каталоге"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil)
					;                (test-hash1 (make-hash))
					;                (test-hash2 (make-hash)))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash1 'name "firstfile.txt")
					;            (set-hash test-hash1 'size 96)
					;            (set-hash test-hash1 'first-block 200)
					;            (set-hash test-hash1 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash1 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash1 'access-date (list 1991 6 25))
					;            (set-hash test-hash1 'attributes '())
					;            (set-hash test-hash1 'parent-first-block nil)
					;            (set-hash test-hash1 'short-name "karas.txt")
					;            (set-hash test-hash1 'dir nil)

					;            (set-hash test-hash2 'name "first dir")
					;            (set-hash test-hash2 'size 96)
					;            (set-hash test-hash2 'first-block 200)
					;            (set-hash test-hash2 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash2 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash2 'access-date (list 1991 6 25))
					;            (set-hash test-hash2 'attributes '(DIRECTORY))
					;            (set-hash test-hash2 'parent-first-block nil)
					;            (set-hash test-hash2 'short-name "dir")
					;            (set-hash test-hash2 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash1
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (set-hash
					;             test-hash2
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (assert (list (fat32-find-entry-pos test-hash1) (fat32-find-entry-pos test-hash2))
					; 		   '((0 . 32) (0 . 96)))))

					; (deftest fat32-update-entry-test () "update entry test"
					;          "Проверка обновления записи в каталоге без именения количества записей длинного имени"
					;          nil)

					; (deftest fat32-get-free-entry-pos-test () "get free entry pos test"
					;          "Проверка получения позиции где смогут поместиться 2 записи"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (assert (fat32-get-free-entry-pos
					;                     (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block)
					;                     2)
					; 		   '(0 . 128))))

					; (deftest fat32-mark-for-delete-test () "mark entries for delete test"
					;          "Проверка пометки записей на удаление"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (fat32-mark-for-delete test-hash)
					;            (setq test-sector (block-read 2))
					;            (assert (list (aref test-sector 64) (aref test-sector 96))
					; 		   (list +fat32-entry-deleted+ +fat32-entry-deleted+))))

					; (deftest get-free-block-test () "get free block test"
					;          "Проверка получения номера первого свободного блока"
					;          (let ((fat-table-small (make-array 256)))
					;            (for i 0 256 (seta fat-table-small i +fat-block-free+))
					;            (seta fat-table-small 2 +fat-block-end+)
					;            (seta fat-table-small 3 5)
					;            (seta fat-table-small 5 16)
					;            (seta fat-table-small 16 150)
					;            (seta fat-table-small 150 151)
					;            (seta fat-table-small 151 +fat-block-end+)

					;            (seta fat-table-small 200 152)
					;            (seta fat-table-small 152 127)
					;            (seta fat-table-small 127 160)
					;            (seta fat-table-small 160 9)
					;            (seta fat-table-small 9 100)
					;            (seta fat-table-small 100 +fat-block-end+)
					;            (assert (get-free-block fat-table-small 1)
					; 		   4)))

					; (deftest update-fat-elem-test1 () "update fat elem test1"
					;          "Проверка установки значения элемента таблицы FAT на диске"
					;          (let ((bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector1 nil)
					;                (test-sector2 nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (update-fat-elem 5 +fat-block-end+)
					;            (setq test-sector1 (ata-read-sectors *disk* 3 1))
					;            (setq test-sector2 (ata-read-sectors *disk* 5 1))
					;            (assert (list (arr-get-num test-sector1 20 4)
					;                          (arr-get-num test-sector2 20 4))
					; 		   (list +fat-block-end+
					;                          +fat-block-end+))))

					; (deftest update-fat-elem-test2 () "update fat elem test2"
					;          "Проверка установки значения элемента таблицы FAT на диске"
					;          (let ((bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector1 nil)
					;                (test-sector2 nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (FAT32FileSystem-set-fat-copying *file-system* nil)
					;            (update-fat-elem 5 +fat-block-end+)
					;            (setq test-sector1 (ata-read-sectors *disk* 3 1))
					;            (setq test-sector2 (ata-read-sectors *disk* 5 1))
					;            (assert (list (arr-get-num test-sector1 20 4)
					;                          (arr-get-num test-sector2 20 4))
					; 		   (list +fat-block-end+
					; 			 16))))

					; (deftest append-fat-chain-test () "append fat chain test"
					;          "Проверка добавления элемента в цепочку блоков"
					;          (let ((bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector1 nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (append-fat-chain 2 90)
					;            (setq test-sector1 (ata-read-sectors *disk* 3 1))
					;            (assert (list (arr-get-num test-sector1 8 4)
					;                          (get-hash (FAT32FileSystem-fat-table *file-system*) 2))
					; 		   (list 90
					; 			 '(90)))))

					; (deftest fat32-update-entry-test1 () "update entry test1"
					;          "Проверка обновления записи в каталоге без изменения количества записей длинного имени"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (fat32-update-entry test-hash '((size . 100)))
					;            (setq test-sector (block-read 2))
					;            (assert (list (aref test-sector 124)
					;                          (get-hash test-hash 'size))
					; 		   (list 100
					; 			 100))))

					; (deftest fat32-update-entry-test2 () "update entry test2"
					;          "Проверка обновления записи в каталоге с изменением количества записей длинного имени"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (fat32-update-entry test-hash '((name . "more-than-13-symbols")))
					;            (setq test-sector (block-read 2))
					;            (assert (list (get-hash test-hash 'name)
					;                          (car (funcall
					;                                (parse-lfn-entry)
					;                                (stream-from-arr
					;                                 (array-seq test-sector 64 96)
					;                                 nil)))
					;                          (car (funcall
					;                                (parse-lfn-entry)
					;                                (stream-from-arr
					;                                 (array-seq test-sector 96 128)
					;                                 nil))))
					; 		   (list "more-than-13-symbols"
					;                          "symbols"
					;                          "more-than-13-"))))

					; (deftest new-block-test () "new block test"
					;          "Проверка выделение нового блока для цепочки блоков а также подсчёт количества свободных блоков и последнего свободного блока"
					;          (let ((bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (new-block *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (setq test-sector (ata-read-sectors *disk* 1 1))
					;            (assert (list (get-fat-chain 2)
					;                          (FAT32FileSystem-free-blocks-count *file-system*)
					;                          (FAT32FileSystem-free-block-num *file-system*))
					; 		   (list '(2 4)
					; 			 241
					; 			 4))
					;            (funcall (parse-fsinfo) (stream-from-arr test-sector nil))
					;            (assert (list (get-fat-chain 2)
					;                          (FAT32FileSystem-free-blocks-count *file-system*)
					;                          (FAT32FileSystem-free-block-num *file-system*))
					; 		   (list '(2 4)
					; 			 241
					; 			 4))))

					; (deftest fat32-generate-short-name-test1 () "generate short name test 1"
					;          "Проверка генерации короткого имени"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "dir")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (assert (fat32-generate-short-name (FAT32FileSystem-root-entry *file-system*) "file.bin") "FILE~1.BIN")))

					; (deftest fat32-generate-short-name-test2 () "generate short name test 2"
					;          "Проверка генерации короткого имени"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "first dir")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "DIR")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (rplaca
					;             (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)
					;             (append (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))
					;                     (list test-hash)))
					;            (assert (fat32-generate-short-name (FAT32FileSystem-root-entry *file-system*) "dir") "DIR~2")))

					; (deftest fat32-add-entry-test () "fat32 add entry"
					;          "Проверка добавление записи на диск"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-hash (make-hash))
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "more-than-13-symbols")
					;            (set-hash test-hash 'size 96)
					;            (set-hash test-hash 'first-block 200)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes '(DIRECTORY))
					;            (set-hash test-hash 'parent-first-block nil)
					;            (set-hash test-hash 'short-name "DIR")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash
					;             test-hash
					;             'parent-first-block
					;             (car (get-fat-chain (get-hash (FAT32FileSystem-root-entry *file-system*) 'first-block))))
					;            (fat32-add-entry test-hash)
					;            (set-hash test-hash 'name "DIR")
					;            (set-hash test-hash 'parent-first-block ())
					;            (setq test-sector (block-read 2))

					;            (assert (list (car (funcall
					;                                (parse-lfn-entry)
					;                                (stream-from-arr
					;                                 (array-seq test-sector 128 160)
					;                                 nil)))
					;                          (car (funcall
					;                                (parse-lfn-entry)
					;                                (stream-from-arr
					;                                 (array-seq test-sector 160 192)
					;                                 nil)))
					;                          (car (funcall
					;                                (parse-fat32-dir-entry)
					;                                (stream-from-arr
					;                                 (array-seq test-sector 192 224)
					;                                 nil))))
					; 		   (list "symbols"
					; 			 "more-than-13-"
					;                          test-hash))))

					; (deftest rename-test () "rename test"
					;          "Проверка переименовывания файла"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (rename *file-system* (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) "Karas")
					;            (assert (list (get-hash (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) 'name))
					; 		   (list "Karas"))))

					; (deftest remove-file-test () "remove file test"
					;          "Проверка на пометку файла на удаление"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (remove-file
					;             *file-system*
					;             (FAT32FileSystem-root-entry *file-system*)
					;             (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					;            (setq test-sector (block-read 2))
					;            (assert (list (list-length (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					;                          (aref test-sector 32))
					; 		   (list 1
					; 			 0xE5))))

					; (deftest remove-dir-test () "remove dir test"
					;          "Проверка на пометку файла на удаление"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (set-hash (cadar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) 'dir '(()))
					;            (remove-dir
					;             *file-system*
					;             (FAT32FileSystem-root-entry *file-system*)
					;             (cadar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					;            (setq test-sector (block-read 2))
					;            (assert (list (list-length (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					;                          (aref test-sector 96))
					; 		   (list 1
					; 			 0xE5))))

					; (deftest free-space-test () "free space test"
					;          "Проверка на получение свободного места"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*))
					;              (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (assert (free-space *file-system*)
					;                    242)))

					; (deftest create-file-test () "create file test"
					;          "Проверка создания файла"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil)
					;                (test-hash (make-hash)))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "new-file.bin")
					;            (set-hash test-hash 'size 0)
					;            (set-hash test-hash 'first-block 4)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes ())
					;            (set-hash test-hash 'parent-first-block 2)
					;            (set-hash test-hash 'short-name "NEW-FI~1.BIN")
					;            (set-hash test-hash 'dir nil)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (create-file *file-system* (FAT32FileSystem-root-entry *file-system*) "new-file.bin")
					;            (set-hash test-hash 'creation-date-time (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'creation-date-time))
					;            (set-hash test-hash 'modify-date-time (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'modify-date-time))
					;            (set-hash test-hash 'access-date (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'access-date))
					;            (assert (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					; 		   test-hash)))

					; (deftest create-dir-test () "create dir test"
					;          "Проверка создания каталога"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil)
					;                (test-hash (make-hash)))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (set-hash test-hash 'name "new-file.bin")
					;            (set-hash test-hash 'size 0)
					;            (set-hash test-hash 'first-block 4)
					;            (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
					;            (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
					;            (set-hash test-hash 'access-date (list 1991 6 25))
					;            (set-hash test-hash 'attributes ())
					;            (set-hash test-hash 'parent-first-block 2)
					;            (set-hash test-hash 'short-name "NEW-FI~1.BIN")
					;            (set-hash test-hash 'dir t)

					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;            (create-dir *file-system* (FAT32FileSystem-root-entry *file-system*) "new-file.bin")
					;            (set-hash test-hash 'creation-date-time (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'creation-date-time))
					;            (set-hash test-hash 'modify-date-time (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'modify-date-time))
					;            (set-hash test-hash 'access-date (get-hash (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))) 'access-date))
					;            (assert (last (car (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)))
					; 		   test-hash)))

					; (deftest open-file-test () "open file test"
					;          "Проверка открытия файла"
					;          (let ((lfn-entry1 *lfn-entry1*)
					;                (lfn-entry2 *lfn-entry2*)
					;                (dir-entry1 *dir-entry1*)
					;                (dir-entry2 *dir-entry2*)
					;                (bios-block *bios-parameter-block*)
					;                (fsinfo-struct *fsinfo-struct*)
					;                (fat-table-sectors *fat-table-sectors*)
					;                (test-sector nil))
					;                (setq *file-system* (make-instance 'FAT32FileSystem))
					;            (ata-write-sectors *disk* 0 1 bios-block)
					;            (ata-write-sectors *disk* 1 1 fsinfo-struct)
					;            (ata-write-sectors *disk* 2 1 bios-block)
					;            (ata-write-sectors *disk* 3 2 fat-table-sectors)
					;            (ata-write-sectors *disk* 5 2 fat-table-sectors)
					;            (fs-init *file-system* *disk* 0 65535)
					;            (setq test-sector (make-array 1920))
					;            (for i 0 1920 (seta test-sector i 0))
					;            (setq test-sector (array-cat (array-cat lfn-entry2 dir-entry2)
					;                                         test-sector))
					;            (setq test-sector (array-cat (array-cat lfn-entry1 dir-entry1)
					;                                         test-sector))
					;            (block-write 2 test-sector)
					;            (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
					;              (setq test-file (open-file *file-system*
					;                                         (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))
					;              (assert
					;                  (list
					;                   (FAT32File-name test-file)
					;                   (FAT32File-size test-file)
					;                   (FAT32File-position test-file)
					;                   (FAT32File-blocks test-file)
					;                   (FAT32File-dir-entry test-file))
					;                  (list
					;                   "firstfile.txt"
					;                   96
					;                   '(0 . 0)
					;                   '(200 152 127 160 9 100)
					;                   (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))))

(deftest read-file-test () "read file test"
         "Проверка чтения файла"
         (let ((lfn-entry1 *lfn-entry1*)
               (lfn-entry2 *lfn-entry2*)
               (dir-entry1 *dir-entry1*)
               (dir-entry2 *dir-entry2*)
               (bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (test-sector nil)
               (test-hash (make-hash))
               (test-buf nil))
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
           (setq test-sector (make-array 2048))
           (for i 0 2048 (seta test-sector i 32))
           (block-write 200 test-sector)
           (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
           (setq test-hash (copy-tree (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))
           (setq test-file (open-file *file-system*
                                      (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))
           (setq test-buf (read-file test-file 96))
           (assert (list
                    test-buf
                    (not (equal (get-hash (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) 'access-date) (get-hash test-hash 'access-date)))
                    )
                   (list
                    (array-seq test-sector 0 96)
                    t))))

(deftest write-file-test () "write file test"
         "Проверка записи в файл"
         (let ((lfn-entry1 *lfn-entry1*)
               (lfn-entry2 *lfn-entry2*)
               (dir-entry1 *dir-entry1*)
               (dir-entry2 *dir-entry2*)
               (bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (test-sector nil)
               (test-hash (make-hash))
               (test-buf nil))
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
           (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))
           (setq test-hash (copy-tree (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))
           (setq test-file (open-file *file-system*
                                      (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir))))
           (setq test-sector (make-array 96))
           (for i 0 96 (seta test-sector i 32))
           (write-file test-file test-sector)
           (seek-file test-file -96 'CUR)
           (setq test-buf (read-file test-file 96))
           (assert (list
                    test-buf
                    (not (equal (get-hash (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) 'access-date) (get-hash test-hash 'access-date)))
                    (not (equal (get-hash (caar (get-hash (FAT32FileSystem-root-entry *file-system*) 'dir)) 'modify-date-time) (get-hash test-hash 'modify-date-time))))
                   (list
                    (array-seq test-sector 0 96)
                    t
                    t))))

