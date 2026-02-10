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
(defvar *lfn-entry1* #(41
                       0x66 0 0x69 0 0x72 0 0x73 0 0x74 0
                       0xF
                       0
                       1
                       0x66 0 0x69 0 0x6c 0 0x65 0 0x2e 0 0x74 0
                       0 0
                       0x78 0 0x74 0))

;;first dir
(defvar *lfn-entry2* #(41
                       0x66 0 0x69 0 0x72 0 0x73 0 0x74 0
                       0xF
                       0
                       1
                       0x20 0 0x64 0 0x69 0 0x72 0 0xFF 0 0xFF 0
                       0 0
                       0xFF 0 0xFF 0))

(defvar *dir-entry1* #(0x64 0x69 0x72 0x20 0x20 0x20 0x20 0x20 0x20 0x20 0x20
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

(deftest parse-bios-parameter-block-test () ()
         "Проверка парсера блока параметров биос"
         (let ((bios-block *bios-parameter-block*)
               (fat32fs (make-instance 'FAT32FileSystem))
               (root-entry (make-hash)))
           (set-hash root-entry 'name 'root)
           (set-hash root-entry 'size -1)
           (set-hash root-entry 'blocks (list 2))
           (set-hash root-entry 'creation-date-time nil)
           (set-hash root-entry 'modify-date-time nil)
           (set-hash root-entry 'access-date nil)
           (set-hash root-entry 'attributes '(DIRECTORY))
           (set-hash root-entry 'dir t)
           (funcall (parse-bios-parameter-block fat32fs) (stream-from-arr bios-block nil))
           (assert
            (list (FAT32FileSystem-fat-start-sectors fat32fs)
                  (FAT32FileSystem-fat-sectors fat32fs)
                  (FAT32FileSystem-fat-active-num fat32fs)
                  (FAT32FileSystem-fat-copying fat32fs)
                  (FAT32FileSystem-fsinfo-sector fat32fs)
                  (FAT32FileSystem-volume-size fat32fs)
                  (FAT32FileSystem-root-entry fat32fs))
            (list #(3 5)
                  2
                  0
                  t
                  1
                  65535
                  root-entry))))

(deftest parse-fsinfo-test () ()
         "Проверка парсера структуры FSInfo"
         (let ((fsinfo-struct *fsinfo-struct*)
               (fat32fs (make-instance 'FAT32FileSystem)))
           (funcall (parse-fsinfo fat32fs) (stream-from-arr fsinfo-struct nil))
           (assert
            (list (FAT32FileSystem-free-blocks-count fat32fs)
                  (FAT32FileSystem-free-block-num fat32fs))
            (list 0xffffffff
                  2))))

(deftest fat32-get-date-time-test-1 () ()
         "Проверка получения списка даты и времени для времени создания"
         (assert (fat32-get-date-time #(0xD9 0x16) #(0xD6 0x63) #(120))
		 (list 1991
                       6
                       25
                       12
                       30
                       45
                       200)))

(deftest fat32-get-date-time-test-2 () ()
         "Проверка получения списка даты и времени для времени последней модификации"
         (assert (fat32-get-date-time #(0xD9 0x16) #(0xD6 0x63))
		 (list 1991
                       6
                       25
                       12
                       30
                       44)))

(deftest fat32-get-date-time-test-3 () ()
         "Проверка получения списка даты и времени для времени последнего доступа"
         (assert (fat32-get-date-time #(0xD9 0x16) nil)
		 (list 1991
                       6
                       25
                       0
                       0
                       0)))

(deftest fat32-get-attributes-test-1 () ()
         "Проверка получения списка аттрибутов из байта"
         (assert (fat32-get-attributes #(38))
		 (list 'ARCHIVE 'SYSTEM 'HIDDEN)))

(deftest fat32-get-attributes-test-2 () ()
         "Проверка получения списка аттрибутов из байта"
         (assert (fat32-get-attributes #(19))
		 (list 'DIRECTORY 'HIDDEN 'READ-ONLY)))

(deftest get-fat-chain-test () ()
         "Проверка получения списка цепочек таблицы FAT"
         (let ((fat-table-sectors *fat-table-sectors*)
               (fat32fs (make-instance 'FAT32FileSystem)))
           (FAT32FileSystem-set-fat-active-num fat32fs 0)
           (FAT32FileSystem-set-fat-sectors fat32fs 2)
           (FAT32FileSystem-set-fat-start-sectors fat32fs #(5))
           (FAT32FileSystem-set-fat-table fat32fs (make-hash))
           (ata-write-sectors *disk* 5 2 fat-table-sectors)
           (assert (list (get-fat-chain fat32fs 2)
                         (get-fat-chain fat32fs 3)
                         (get-fat-chain fat32fs 200))
		   (list '(2)
			 '(3 5 16 150 151)
			 '(200 152 127 160 9 100)))))

(deftest parse-lfn-entry-test1 () ()
         "Проверка парсера структуры записи длинного имени"
         (let ((lfn-entry *lfn-entry1*))
           (assert (car (funcall (parse-lfn-entry) (stream-from-arr lfn-entry nil)))
		   "firstfile.txt")))

(deftest parse-lfn-entry-test2 () ()
         "Проверка парсера структуры записи длинного имени"
         (let ((lfn-entry *lfn-entry1*))
           (assert (car (funcall (parse-lfn-entry) (stream-from-arr lfn-entry nil)))
		   "firstfile.txt")))

(deftest parse-fat32-dir-entry-test () ()
         "Проверка парсера структуры записи в каталоге"
         (let ((dir-entry *dir-entry1*)
               (bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (fat32fs (make-instance 'FAT32FileSystem))
               (test-hash (make-hash)))
           (set-hash test-hash 'name "dir")
           (set-hash test-hash 'size 96)
           (set-hash test-hash 'blocks '(200 152 127 160 9 100))
           (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
           (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
           (set-hash test-hash 'access-date (list 1991 6 25 0 0 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (set-hash test-hash 'short-name "dir")

           (ata-write-sectors *disk* 0 1 bios-block)
           (ata-write-sectors *disk* 1 1 fsinfo-struct)
           (ata-write-sectors *disk* 2 1 bios-block)
           (ata-write-sectors *disk* 3 2 fat-table-sectors)
           (ata-write-sectors *disk* 5 2 fat-table-sectors)
           (fs-init fat32fs *disk* 0 65535)
           (assert (car (funcall (parse-fat32-dir-entry fat32fs) (stream-from-arr dir-entry nil)))
		   test-hash)))

(deftest fs-init-test () ()
         "Проверка загруки файловой системы с диска"
         (let ((bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (fat32fs (make-instance 'FAT32FileSystem))
               (root-entry (make-hash)))
           (set-hash root-entry 'name 'root)
           (set-hash root-entry 'size 2048)
           (set-hash root-entry 'blocks (list 2))
           (set-hash root-entry 'creation-date-time nil)
           (set-hash root-entry 'modify-date-time nil)
           (set-hash root-entry 'access-date nil)
           (set-hash root-entry 'attributes '(DIRECTORY))
           (set-hash root-entry 'dir t)
           (ata-write-sectors *disk* 0 1 bios-block)
           (ata-write-sectors *disk* 1 1 fsinfo-struct)
           (ata-write-sectors *disk* 2 1 bios-block)
           (ata-write-sectors *disk* 3 2 fat-table-sectors)
           (ata-write-sectors *disk* 5 2 fat-table-sectors)
           (fs-init fat32fs *disk* 0 65535)
           (assert
            (list (FAT32FileSystem-fat-start-sectors fat32fs)
                  (FAT32FileSystem-fat-sectors fat32fs)
                  (FAT32FileSystem-fat-active-num fat32fs)
                  (FAT32FileSystem-fat-copying fat32fs)
                  (FAT32FileSystem-fsinfo-sector fat32fs)
                  (FAT32FileSystem-volume-size fat32fs)
                  (FAT32FileSystem-root-entry fat32fs)
                  (FAT32FileSystem-free-blocks-count fat32fs)
                  (FAT32FileSystem-free-block-num fat32fs))
            (list #(3 5)
                  2
                  0
                  t
                  1
                  65535
                  root-entry
                  0xffffffff
                  2))))

(deftest load-dir-test1 () ()
         "Проверка раскрытия и сохранения содержимого корневого каталога"
         (let ((lfn-entry *lfn-entry2*)
               (dir-entry *dir-entry1*)
               (bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (fat32fs (make-instance 'FAT32FileSystem))
               (test-sector nil)
               (test-hash (make-hash)))

           (set-hash test-hash 'name "first dir")
           (set-hash test-hash 'size 96)
           (set-hash test-hash 'blocks '(200 152 127 160 9 100))
           (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
           (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
           (set-hash test-hash 'access-date (list 1991 6 25 0 0 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (set-hash test-hash 'short-name "dir")
           (ata-write-sectors *disk* 0 1 bios-block)
           (ata-write-sectors *disk* 1 1 fsinfo-struct)
           (ata-write-sectors *disk* 2 1 bios-block)
           (ata-write-sectors *disk* 3 2 fat-table-sectors)
           (ata-write-sectors *disk* 5 2 fat-table-sectors)
           (fs-init fat32fs *disk* 0 65535)
           (setq test-sector (make-array 1984))
           (for i 0 1984 (seta test-sector i 0))
           (setq test-sector (array-cat (array-cat lfn-entry dir-entry)
                                        test-sector))
           (block-write 2 test-sector)
           (load-dir fat32fs (FAT32FileSystem-root-entry fat32fs))
           (assert (get-hash (FAT32FileSystem-root-entry fat32fs) 'dir)
                   (list test-hash))))

(deftest load-dir-test2 () ()
         "Проверка раскрытия и сохранения содержимого корневого каталога c несколькими записями длинного имени"
         (let ((lfn-entry1 *lfn-entry1*)
               (lfn-entry2 *lfn-entry2*)
               (dir-entry *dir-entry1*)
               (bios-block *bios-parameter-block*)
               (fsinfo-struct *fsinfo-struct*)
               (fat-table-sectors *fat-table-sectors*)
               (fat32fs (make-instance 'FAT32FileSystem))
               (test-sector nil)
               (test-hash (make-hash)))
           (set-hash test-hash 'name "first dirfirstfile.txt")
           (set-hash test-hash 'size 96)
           (set-hash test-hash 'blocks '(200 152 127 160 9 100))
           (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
           (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
           (set-hash test-hash 'access-date (list 1991 6 25 0 0 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (set-hash test-hash 'short-name "dir")
           (ata-write-sectors *disk* 0 1 bios-block)
           (ata-write-sectors *disk* 1 1 fsinfo-struct)
           (ata-write-sectors *disk* 2 1 bios-block)
           (ata-write-sectors *disk* 3 2 fat-table-sectors)
           (ata-write-sectors *disk* 5 2 fat-table-sectors)
           (fs-init fat32fs *disk* 0 65535)
           (setq test-sector (make-array 1952))
           (for i 0 1952 (seta test-sector i 0))
           (setq test-sector (array-cat (array-cat (array-cat lfn-entry1 lfn-entry2) dir-entry) test-sector))
           (block-write 2 test-sector)
           (load-dir fat32fs (FAT32FileSystem-root-entry fat32fs))
           (assert (get-hash (FAT32FileSystem-root-entry fat32fs) 'dir)
                   (list test-hash))))

(deftest fstat-test () ()
         "Проверка получения метаданных каталога"
         (let ((test-hash (make-hash))
               (test-stat (make-hash))
               (fat32fs (make-instance 'FAT32FileSystem)))
           (set-hash test-hash 'name "first dirfirstfile.txt")
           (set-hash test-hash 'size 96)
           (set-hash test-hash 'blocks '(200 152 127 160 9 100))
           (set-hash test-hash 'creation-date-time (list 1991 6 25 12 30 45 200))
           (set-hash test-hash 'modify-date-time (list 1991 6 25 12 30 44))
           (set-hash test-hash 'access-date (list 1991 6 25 0 0 0))
           (set-hash test-hash 'attributes '(DIRECTORY))
           (set-hash test-hash 'dir t)
           (set-hash test-hash 'short-name "dir")
           (set-hash test-stat 'name "first dirfirstfile.txt")
           (set-hash test-stat 'size 0)
           (set-hash test-stat 'create-date '(25 6 1991))
           (set-hash test-stat 'create-time '(12 30 45))
           (set-hash test-stat 'modify-date '(25 6 1991))
           (set-hash test-stat 'modify-time '(12 30 44))
           (set-hash test-stat 'access-date '(25 6 1991))
           (set-hash test-stat 'access-time nil)
           (set-hash test-stat 'isdir t)
           (set-hash test-stat 'create-time-ms 200)
           (set-hash test-stat 'flags '(DIRECTORY))
           (assert (fstat fat32fs test-hash) test-stat)))
