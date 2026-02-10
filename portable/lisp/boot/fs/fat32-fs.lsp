(defconst +fat-block-free+ 0) ;; блок свободен
(defconst +fat-block-damaged+ 268435447) ;; 0xFFFFFF7 блок повреждён
(defconst +fat-block-end+ 268435455) ;; 0xFFFFFFF блок является концом цепочки

;; Класс файловой системы FAT32
(defclass FAT32FileSystem FileSystem (fat-table fat-start-sectors fat-sectors fat-active-num fat-copying fsinfo-sector free-blocks-count free-block-num volume-size root-entry))

(defun parse-bios-parameter-block (fat32fs)
  "Парсер блока параметров BIOS"
  (&&& bpb-struct-> (parse-struct '((jmp-command . 3)
                                    (oem-id . 8)
                                    (bytes-per-sector . 2) ;; must be 512
                                    (sectors-per-block . 1) ;; for block size
                                    (reserved-sectors-count . 2) ;; boot-record + padding size in sectors before FAT
                                    (fat-tables-count . 1) ;; must be 2
                                    (root-dir-etries-count . 2)
                                    (volume-size-small . 2) ;; size of volume in lba-sectors if less than 65536 else 0
                                    (media-descriptor-type . 1)
                                    (sectors-per-fat-fat16 . 2)
                                    (sectors-per-track . 2)
                                    (heads-count . 2)
                                    (hidden-sectors-count . 4)
                                    (volume-size-large . 4) ;; size of volume in lba-sectors if more than 65535 else 0
                                    (sectors-per-fat-fat32 . 4) ;; for fat table size
                                    (active-fat-struct . 2) ;; 4 bit for active fat num + 3 bit reserved + 1 bit if fat copying + 8 bit reserved
                                    (fat-version . 2)
                                    (root-entry-first-block . 4) ;; for root entry
                                    (fsinfo-sector . 2) ;; fsinfo sector number (must be before fat table)
                                    (bpb-copy-sector . 2)
                                    (reserved1 . 12)
                                    (disk-type-num . 1)
                                    (reserved2 . 1)
                                    (volume-signature . 1) ;; 41(0x29) if volume-serial or volume-label are not zero else 40(0x28)
                                    (volume-serial . 4)
                                    (volume-label . 11)
                                    (system-id . 8)
                                    (reserved3 . 420)
                                    (bpb-end-signature . 2))) ;; must be 43605(aa55)
       return (progn
		(unless (= 512 (arr-get-num (get-hash bpb-struct 'bytes-per-sector) 0 2))
                  (throw 'error "parse-bios-parameter-block: bytes per sector in bpb is not 512"))
		(let ((fat-tables-count (arr-get-num (get-hash bpb-struct 'fat-tables-count) 0 1))
                      (active-fat-struct (arr-get-num (get-hash bpb-struct 'active-fat-struct) 0 2)))
                  (FAT32FileSystem-set-fat-start-sectors fat32fs (make-array fat-tables-count))
                  (FAT32FileSystem-set-fat-sectors fat32fs (arr-get-num (get-hash bpb-struct 'sectors-per-fat-fat32) 0 4))
                  (for i 0 fat-tables-count
                       (seta (FAT32FileSystem-fat-start-sectors fat32fs) i (+ (arr-get-num (get-hash bpb-struct 'reserved-sectors-count) 0 2) (* i (FAT32FileSystem-fat-sectors fat32fs)))))
                  (FAT32FileSystem-set-fat-active-num fat32fs (& active-fat-struct 15))
                  (FAT32FileSystem-set-fat-copying fat32fs (if (= (get-bit-from-num active-fat-struct 7) 0) t nil)))
		(set-block-size (arr-get-num (get-hash bpb-struct 'sectors-per-block) 0 1))
		(let ((volume-size-small (arr-get-num (get-hash bpb-struct 'volume-size-small) 0 2))
                      (volume-size-large (arr-get-num (get-hash bpb-struct 'volume-size-large) 0 4)))
                  (if (= volume-size-small 0) (FAT32FileSystem-set-volume-size fat32fs volume-size-large)
                      (FAT32FileSystem-set-volume-size fat32fs volume-size-small)))
		(set-block-offset (- (+
                                      (aref (FAT32FileSystem-fat-start-sectors fat32fs)
					    (- (array-size (FAT32FileSystem-fat-start-sectors fat32fs)) 1))
                                      (FAT32FileSystem-fat-sectors fat32fs))
                                     (* 2 *block-sectors*)))
		(set-blocks-start-num 2)
		(FAT32FileSystem-set-fsinfo-sector fat32fs (arr-get-num (get-hash bpb-struct 'fsinfo-sector) 0 2))
		(let ((root-entry (make-hash)))
                  (set-hash root-entry 'name 'root)
                  (set-hash root-entry 'size -1)
                  (set-hash root-entry 'blocks (list (arr-get-num (get-hash bpb-struct 'root-entry-first-block) 0 4)))
                  (set-hash root-entry 'creation-date-time nil)
                  (set-hash root-entry 'modify-date-time nil)
                  (set-hash root-entry 'access-date nil)
                  (set-hash root-entry 'attributes '(DIRECTORY))
                  (set-hash root-entry 'dir t)
                  (FAT32FileSystem-set-root-entry fat32fs root-entry)))))

(defun parse-fsinfo (fat32fs)
  "Парсер структуры FSInfo"
  (&&& fsinfo-struct->(parse-struct '((struct-signature . 4) ;; 1096897106(0x41615252)
				      (reserved1 . 480)
				      (check-signature . 4) ;; 1631679090(0x61417272)
				      (free-blocks-count . 4) ;; 4294967295(0xFFFFFFFF) если посчитать
				      (free-block-num . 4) ;; 4294967295(0xFFFFFFFF) если посчитать
				      (reserved2 . 12)
				      (end-signature . 4))) ;; 43605(0xAA550000)
       return (progn
		(FAT32FileSystem-set-free-blocks-count fat32fs (arr-get-num (get-hash fsinfo-struct 'free-blocks-count) 0 4))
		(FAT32FileSystem-set-free-block-num fat32fs (arr-get-num (get-hash fsinfo-struct 'free-block-num) 0 4)))))

(defun fat32-get-date-time (date-bytes time-bytes &rest creation-time-ms)
  "Получить список даты и времени из массивов байт date-bytes, time-bytes и creation-time-ms"
  (let ((date-num (arr-get-num date-bytes 0 2))
        (time-num (when (not (null time-bytes)) (arr-get-num time-bytes 0 2)))
        (time-ms-num (when (not (null creation-time-ms)) (arr-get-num (car creation-time-ms) 0 1)))
        (sec-plus 0)
        (date-time nil))
    (when (and (not (null time-ms-num)) (not (null time-num)) (>= time-ms-num 100))
      (setq sec-plus 1)
      (setq time-ms-num (- time-ms-num 100)))
    (setq date-time (list (+ (>> (& date-num 0xFE00) 9) 1980) (>> (& date-num 0x1E0) 5) (& date-num 0x1F)))
    (if (not (null time-num))
        (setq date-time (append date-time (list (>> (& time-num 0xF800) 11) (>> (& time-num 0x7E0) 5) (+ (* (& time-num 0x1F) 2) sec-plus))))
        (setq date-time (append date-time '(0 0 0))))
    (when (not (null time-ms-num))
      (setq date-time (append date-time (list (* time-ms-num 10)))))
    date-time))

(defun fat32-get-attributes (attribute-byte)
  "Получить список аттрибутов из записи в каталоге из байта attribute-byte"
  (let ((attributes nil)
        (all-attributes #(READ-ONLY HIDDEN SYSTEM VOLUME-ID DIRECTORY ARCHIVE nil nil)))
    (setq attribute-byte (arr-get-num attribute-byte 0 1))
    (for i 0 8
         (unless (or (= i 6) (= i 7))
           (unless (= 0 (& attribute-byte (expt 2 i)))
             (setq attributes (cons (aref all-attributes i) attributes)))))
    attributes))

(defun fat-elem-pos (idx)
  "Получить пару из смещение сектора + смещение в секторе для элемента с индексом idx таблицы FAT"
  (cons (/ (* idx 4) +sector-size+) (% (* idx 4) +sector-size+))) ;; TODO Запись при new-block на диск

(defun get-fat-chain (fat32fs block-num)
  "Получить цепочку блоков из таблицы FAT, начиная с блока block-num"
  (let ((fat-hash (FAT32FileSystem-fat-table fat32fs))
        (fat-table nil)
        (cur-elem nil)
        (prev-elem nil)
        (fat-chain nil))
    (when (>= block-num (/ (* (FAT32FileSystem-fat-sectors fat32fs) +sector-size+) 4))
      (throw 'error "get-fat-chain: block-num out of range"))
    (if (check-key fat-hash block-num) (cons block-num (get-hash fat-hash block-num))
        (progn
          (setq fat-table (make-array (/ (* (FAT32FileSystem-fat-sectors fat32fs) +sector-size+) 4)))
          (let ((cur-table-elem 0))
            (for i 0 (FAT32FileSystem-fat-sectors fat32fs)
                 (let ((sector-array (ata-read-sectors *disk* (+ (aref (FAT32FileSystem-fat-start-sectors fat32fs) (FAT32FileSystem-fat-active-num fat32fs)) i) 1))
                       (cur-pos 0))
                   (while (<= cur-pos (- +sector-size+ 4))
                     (let ((fat-elem (arr-get-num sector-array cur-pos 4)))
                       (seta fat-table cur-table-elem fat-elem)
                       (incf cur-table-elem)
                       (setq cur-pos (+ cur-pos 4)))))))
          (setq prev-elem block-num)
          (setq cur-elem (aref fat-table block-num))
          (while (!= cur-elem +fat-block-end+)
            (setq fat-chain (append fat-chain (list prev-elem)))
            (seta fat-table prev-elem -1)
            (setq prev-elem cur-elem
                  cur-elem (aref fat-table cur-elem))
            (when (and (< cur-elem +fat-block-damaged+) (>= cur-elem (* (FAT32FileSystem-fat-sectors fat32fs) (/ +sector-size+ 4))))
              (throw 'error "get-fat-chain: elem in fat-chain is out of range"))
            (when (= cur-elem +fat-block-free+)
              (throw 'error (concat "get-fat-chain: fat table elem in chain is free on block num = " (inttostr prev-elem))))
            (when (= cur-elem +fat-block-damaged+)
              (throw 'error (concat "get-fat-chain: fat table elem in chain is damaged. Block num = " (inttostr prev-elem))))
            (when (= cur-elem -1)
              (throw 'error (concat "get-fat-chain: fat table elem in chain is recursive starting at block num = " (inttostr prev-elem)))))
          (setq fat-chain (append fat-chain (list prev-elem)))
          (set-hash fat-hash block-num (cdr fat-chain))
          fat-chain))))

;; Каждый символ 2 байта
(defun parse-lfn-entry ()
  "Парсер структуры записи длинного имени"
  (&&& lfn-entry->(parse-struct '((lfn-number . 1)
                                  (str-1 . 10)
                                  (attributes . 1) ;; 15(0x0F)
                                  (entry-type . 1) ;; 0
                                  (checksum . 1) ;; из dir-entry
                                  (str-2 . 12)
                                  (reserved . 2) ;; 0
                                  (str-3 . 4)))
       return (progn
		(let ((lfn-string "")
                      (str-ended nil))
                  (for i 0 5
                       (unless str-ended
                         (let ((code (arr-get-num (get-hash lfn-entry 'str-1) (* i 2) 2)))
                           (if (= code 255)
                               (setq str-ended t)
                               (setq lfn-string (concat lfn-string (make-string 1 (code-char code))))))))
                  (for i 0 6
                       (unless str-ended
                         (let ((code (arr-get-num (get-hash lfn-entry 'str-2) (* i 2) 2)))
                           (if (= code 255)
                               (setq str-ended t)
                               (setq lfn-string (concat lfn-string (make-string 1 (code-char code))))))))
                  (for i 0 2
                       (unless str-ended
                         (let ((code (arr-get-num (get-hash lfn-entry 'str-3) (* i 2) 2)))
                           (if (= code 255)
                               (setq str-ended t)
                               (setq lfn-string (concat lfn-string (make-string 1 (code-char code))))))))
                  lfn-string))))

;; Каждый символ 1 байт
(defun parse-fat32-dir-entry (fat32fs)
  "Парсер структур записи в каталоге"
  (&&& fat32-dir-entry->(parse-struct '((short-name . 11)
					(attributes . 1)
					(reserved . 1)
					(creation-time-ms . 1)
					(creation-time . 2) ;; seconds * 2
					(creation-date . 2)
					(access-date . 2)
					(first-block-1 . 2)
					(modification-time . 2)
					(modification-date . 2)
					(first-block-2 . 2)
					(size-bytes . 4)))
       return (progn
		(let ((dir-entry (make-hash))
                      (name-first (car (split #\  (arr-get-str (get-hash fat32-dir-entry 'short-name) 0 8))))
                      (name-second (car (split #\  (arr-get-str (get-hash fat32-dir-entry 'short-name) 8 3)))))
                  (set-hash dir-entry 'name (if (equal name-second "")
						name-first
						(concat (concat name-first ".") name-second)))
                  (set-hash dir-entry 'size (arr-get-num (get-hash fat32-dir-entry 'size-bytes) 0 4))
                  (set-hash dir-entry 'blocks (get-fat-chain fat32fs (arr-get-num (array-cat (get-hash fat32-dir-entry 'first-block-1) (get-hash fat32-dir-entry 'first-block-2)) 0 4)))
                  (set-hash dir-entry 'creation-date-time (fat32-get-date-time (get-hash fat32-dir-entry 'creation-date) (get-hash fat32-dir-entry 'creation-time) (get-hash fat32-dir-entry 'creation-time-ms)))
                  (set-hash dir-entry 'modify-date-time (fat32-get-date-time (get-hash fat32-dir-entry 'modification-date) (get-hash fat32-dir-entry 'modification-time)))
                  (set-hash dir-entry 'access-date (fat32-get-date-time (get-hash fat32-dir-entry 'access-date) nil))
                  (set-hash dir-entry 'attributes (fat32-get-attributes (get-hash fat32-dir-entry 'attributes)))
                  (set-hash dir-entry 'dir (contains (get-hash dir-entry 'attributes) 'DIRECTORY))
                  (set-hash dir-entry 'short-name (get-hash dir-entry 'name))
                  dir-entry))))

(defmethod fs-init ((self FAT32FileSystem) disk start-sector sector-count)
  "Загрузка файловой системы FAT32 с диска disk, начиная с сектора start-sector, с количеством секторов sector-count. Чтение блока параметров BIOS, структуры FSInfo, таблицы FAT."
  (set-block-disk disk)
  (funcall (parse-bios-parameter-block self) (stream-from-arr (ata-read-sectors disk start-sector 1) nil))
  (when (> (FAT32FileSystem-volume-size self) sector-count) (throw 'error "fs-init(fat32): volume size in bpb is higher than volume size in args"))
  (funcall (parse-fsinfo self) (stream-from-arr (ata-read-sectors disk (FAT32FileSystem-fsinfo-sector self) 1) nil))
  (FAT32FileSystem-set-fat-table self (make-hash))
  (let ((root-entry (FAT32FileSystem-root-entry self)))
    (set-hash
     root-entry
     'blocks
     (get-fat-chain self (car (get-hash root-entry 'blocks))))
    (set-hash
     root-entry
     'size
     (* *block-size* (list-length (get-hash root-entry 'blocks)))))
  self)
;; TODO Посчитать free block count и free block num при вызове new-block

(defmethod load-dir ((self FAT32FileSystem) dir)
  "Раскрыть и сохранить содержимое каталога dir"
  (if (or (null (get-hash dir 'dir)) (not (equal (get-hash dir 'dir) t))) nil
      (let ((blocks (get-hash dir 'blocks))
            (dir-list ())
            (block-stream nil)
            (lfn-name ""))
        (while (not (null blocks))
          (setq block-stream (stream-from-arr (block-read (car blocks)) nil))
          (while (not (null block-stream))
            (let ((next-byte (get-byte block-stream)))
              (unless (null next-byte)
                (case (car next-byte)
                  (0xE5 block-stream (cdr (get-array block-stream 32)))
                  (0 (setq block '(nil) block-stream nil))
                  (otherwise
                   (let ((entry-attributes (aref (car (get-array block-stream 12)) 11)))
                     (if (= entry-attributes 0xF)
                         (progn
                           (let ((parse-res (funcall (parse-lfn-entry) block-stream)))
                             (setq lfn-name (concat (car parse-res) lfn-name))
                             (setq block-stream (cdr parse-res))))
                         (progn
                           (let ((parse-res (funcall (parse-fat32-dir-entry self) block-stream)))
                             (set-hash (car parse-res) 'name lfn-name)
                             (setq lfn-name "")
                             (setq dir-list (append dir-list (list (car parse-res)))
                                   block-stream (cdr parse-res)))))))))))
          (setq blocks (cdr blocks)))
        (set-hash dir 'dir dir-list))))

(defmethod fstat ((self FAT32FileSystem) file-hash)
  "Получение метаданных файлового объекта file-hash"
  (let ((stat (make-hash)))
    (set-hash stat 'name (get-hash file-hash 'name))
    (set-hash stat 'size (if (get-hash file-hash 'dir) 0 (get-hash file-hash 'size)))
    (set-hash stat 'create-date (list (nth (get-hash file-hash 'creation-date-time) 2)
                                      (nth (get-hash file-hash 'creation-date-time) 1)
                                      (nth (get-hash file-hash 'creation-date-time) 0)))
    (set-hash stat 'create-time (list (nth (get-hash file-hash 'creation-date-time) 3)
                                      (nth (get-hash file-hash 'creation-date-time) 4)
                                      (nth (get-hash file-hash 'creation-date-time) 5)))
    (set-hash stat 'modify-date (list (nth (get-hash file-hash 'modify-date-time) 2)
                                      (nth (get-hash file-hash 'modify-date-time) 1)
                                      (nth (get-hash file-hash 'modify-date-time) 0)))
    (set-hash stat 'modify-time (list (nth (get-hash file-hash 'modify-date-time) 3)
                                      (nth (get-hash file-hash 'modify-date-time) 4)
                                      (nth (get-hash file-hash 'modify-date-time) 5)))
    (set-hash stat 'access-date (list (nth (get-hash file-hash 'access-date) 2)
                                      (nth (get-hash file-hash 'access-date) 1)
                                      (nth (get-hash file-hash 'access-date) 0)))
    (set-hash stat 'access-time nil)
    (set-hash stat 'isdir (if (null (get-hash file-hash 'dir)) nil t))
    (set-hash stat 'create-time-ms (nth (get-hash file-hash 'creation-date-time) 6))
    (set-hash stat 'flags (get-hash file-hash 'attributes))
    stat))

(defmethod open-file ((self FAT32FileSystem) file-hash)
  "Отркыть файл как поток для чтения и записи"
  (when (not (null (get-hash file-hash 'dir))) (throw 'error "open-file: directories cannot be opened"))
  (let ((file (make-instance 'FAT32File)))
    (FAT32File-set-name file (get-hash file-hash 'name))
    (FAT32File-set-size file (get-hash file-hash 'size))
    (FAT32File-set-position file '(0 . 0))
    (FAT32File-set-blocks file (get-hash file-hash 'blocks))
    (FAT32File-set-dir file (if (null (get-hash file-hash 'dir)) nil t))
    (FAT32File-set-read-only file (contains (get-hash file-hash 'attributes) 'READ-ONLY))
    file)) ;;TODO добавить изменение времени последнего доступа
