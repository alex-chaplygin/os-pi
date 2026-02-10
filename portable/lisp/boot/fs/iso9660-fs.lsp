(defconst +boot-record-num+ 0) ;; Номер дескриптора типа Boot Record
(defconst +primary-desc-num+ 1) ;; Номер дескриптора типа Primary Volume Descriptor
(defconst +terminator-desc-num+ 255) ;; Номер дескриптора типа Volume Descriptor Set Terminator
(defconst +descriptor-sector+ 64) ;; Сектор с которого начинаются дескрипторы
(defconst +descriptor-size+ 4) ;; Размер дескриптора в секторах

;; Класс файловой системы ISO9660
(defclass CDFSFileSystem FileSystem (fs-block-count path-table-size path-table-sector root-entry))

(defun parse-boot-record ()
  "Парсер оставшейся части структуры дескриптора типа Boot Record"
  (parse-struct '((boot-sys-id . 32)
                  (boot-id . 32)
                  (boot-use . 1977))))

(defun parse-primary-desc ()
  "Парсер оставшейся части структуры дескриптора типа Primary Volume Descriptor"
  (parse-struct '((unused1 . 1)
                  (sys-id . 32)
                  (volume-id . 32)
                  (unused2 . 8)
                  (volume-size-lsb . 4)
                  (volume-size-msb . 4)
                  (unused3 . 32)
                  (volume-set-size . 4)
                  (volume-seq-num . 4)
                  (block-size-lsb . 2)
                  (block-size-msb . 2)
                  (path-table-size-lsb . 4)
                  (path-table-size-msb . 4)
                  (path-table-sector-lsb . 4)
                  (path-table-copy-sector-lsb . 4)
                  (path-table-sector-msb . 4)
                  (path-table-copy-sector-msb . 4)
                  (root-dir-entry . 34)
                  (volume-set-id . 128)
                  (published-id . 128)
                  (data-prep-id . 128)
                  (app-id . 128)
                  (copyright-file-id . 37)
                  (abstract-file-id . 37)
                  (bibliographic-file-id . 37)
                  (volume-create-date-time . 17)
                  (volume-modification-date-time . 17)
                  (volume-expiration-date-time . 17)
                  (volume-effective-date-time . 17)
                  (file-struct-version . 1)
                  (unused4 . 1)
                  (app-used . 512)
                  (reserve . 653))))

(defun parse-set-terminator-desc ()
  "Парсер оставшейся части структуры дескриптора типа Volume Descriptor Set Terminator"
  (parse-struct '((reserve . 2041))))

(defun parse-descriptor-data (desc-type)
  "Возврат парсера оставшейся части структуры дескриптора типа desc-type"
  (case desc-type
    (+boot-record-num+ (parse-boot-record))
    (+primary-desc-num+ (parse-primary-desc))
    (+terminator-desc-num+ (parse-set-terminator-desc))))

(defun parse-date-time-entry ()
  "Парсер структуры даты и времени в записи в каталоге"
  (&&& date-time-entry->(parse-struct '((year . 1)
                                        (month . 1)
                                        (day . 1)
                                        (hour . 1)
                                        (minute . 1)
                                        (second . 1)
                                        (offset . 1)))
       return (list (+ (arr-get-num (get-hash date-time-entry 'year) 0 1) 1900)
                    (arr-get-num (get-hash date-time-entry 'month) 0 1)
                    (arr-get-num (get-hash date-time-entry 'day) 0 1)
                    (arr-get-num (get-hash date-time-entry 'hour) 0 1)
                    (arr-get-num (get-hash date-time-entry 'minute) 0 1)
                    (arr-get-num (get-hash date-time-entry 'second) 0 1))))

(defun cdfs-get-date-time-entry (date-time-bytes)
  "Получить список даты и времени из записи в каталоге из массива байт date-time-bytes(размер массива 7)"
  (car (funcall (parse-date-time-entry) (stream-from-arr date-time-bytes nil))))

(defun cdfs-get-attributes-entry (attr-byte)
  "Получить список аттрибутов из записи в каталоге из байта attr-byte"
  (let ((attributes nil)
        (all-attributes #(HIDDEN DIRECTORY ASSOCIATIVE FORMAT-IN-EXT-ATTR PERMISSIONS-IN-EXT-ATTR nil nil NOT-LAST-ENTRY)))
    (for i 0 8
         (unless (or (= i 5) (= i 6))
           (unless (= 0 (& attr-byte (expt 2 i)))
             (setq attributes (cons (aref all-attributes i) attributes)))))
    attributes))

(defun parse-cdfs-dir-entry ()
  "Парсер структуры записи в каталоге"
  (&&& dir-entry1->(parse-struct '((entry-len . 1)
                                   (entry-ext-attr-len . 1)
                                   (extent-block-num-lsb . 4)
                                   (extent-block-num-msb . 4)
                                   (extent-size-lsb . 4)
                                   (extent-size-msb . 4)
                                   (entry-creation-date-time . 7)
                                   (entry-file-flags . 1)
                                   (interleaved-unit-size . 1)
                                   (interleaved-gap-size . 1)
                                   (volume-seq-num . 4)
                                   (file-name-len . 1)))
       dir-entry2->(parse-struct `((file-name . ,(arr-get-num (get-hash dir-entry1 'file-name-len) 0 1))
                                   (entry-padding . ,(if (= (% (arr-get-num (get-hash dir-entry1 'file-name-len) 0 1) 2) 0) 1 0))))
       return (let ((dir-entry (make-hash)))
                (set-hash dir-entry 'name (arr-get-str (get-hash dir-entry2 'file-name) 0 (array-size (get-hash dir-entry2 'file-name))))
                (set-hash dir-entry 'size (arr-get-num (get-hash dir-entry1 'extent-size-lsb) 0 4))
                (set-hash dir-entry 'blocks (list (arr-get-num (get-hash dir-entry1 'extent-block-num-lsb) 0 4)))
                (let ((left-size (get-hash dir-entry 'size))
                      (cur-block (car (get-hash dir-entry 'blocks))))
                  (while (> left-size *block-size*)
                    (setq left-size (- left-size *block-size*)
                          cur-block (+ cur-block 1))
                    (set-hash dir-entry 'blocks (append (get-hash dir-entry 'blocks) (list cur-block)))))
                (set-hash dir-entry 'creation-date-time (cdfs-get-date-time-entry (get-hash dir-entry1 'entry-creation-date-time)))
                (set-hash dir-entry 'attributes (cdfs-get-attributes-entry (arr-get-num (get-hash dir-entry1 'entry-file-flags) 0 1)))
                (set-hash dir-entry 'dir (contains (get-hash dir-entry 'attributes) 'DIRECTORY))
                dir-entry)))

(defun parse-descriptor (cdfsfs)
  "Парсер структура основы дескриптора"
  (&&&
   descriptor1->(parse-struct '((desc-type . 1)
                                (desc-id . 5)
                                (desc-ver . 1)))
   descriptor2->(parse-descriptor-data (arr-get-num (get-hash descriptor1 'desc-type) 0 1))
   return (case (arr-get-num (get-hash descriptor1 'desc-type) 0 1)
            (+boot-record-num+ t)
            (+terminator-desc-num+ nil)
            (+primary-desc-num+ (progn
                                  (set-block-offset 0)
                                  (set-block-size (/ (arr-get-num (get-hash descriptor2 'block-size-lsb) 0 2) +sector-size+))
                                  (CDFSFileSystem-set-fs-block-count
                                   cdfsfs
                                   (arr-get-num (get-hash descriptor2 'volume-size-lsb) 0 4))
                                  (CDFSFileSystem-set-path-table-size
                                   cdfsfs
                                   (arr-get-num (get-hash descriptor2 'path-table-size-lsb) 0 4))
                                  (CDFSFileSystem-set-path-table-sector
                                   cdfsfs
                                   (arr-get-num (get-hash descriptor2 'path-table-sector-lsb) 0 4))
                                  (CDFSFileSystem-set-root-entry
                                   cdfsfs
                                   (car (funcall (parse-cdfs-dir-entry)
						 (stream-from-arr (get-hash descriptor2 'root-dir-entry) nil))))
                                  (set-hash (CDFSFileSystem-root-entry cdfsfs) 'name "root")
                                  t))
            (otherwise (throw 'error "parse-descriptor: unknown descriptor")))))

(defmethod fs-init ((self CDFSFileSystem) disk start-sector sector-count)
  "Загрузка файловой системы ISO9660 с диска disk, начиная с сектора start-sector, с количеством секторов sector-count. Чтение основного дескриптора тома, записи в каталоге для корневого каталога из основного дескриптора тома."
  (set-block-disk disk)
  (let ((not-terminator t)
        (descriptor-sector (+ start-sector +descriptor-sector+)))
    (while (not (null not-terminator))
      (setq not-terminator
            (car (funcall (parse-descriptor self)
			  (stream-from-arr (ata-read-sectors disk descriptor-sector +descriptor-size+) nil)))
            descriptor-sector (+ descriptor-sector +descriptor-size+)))
    self))  

(defmethod load-dir ((self CDFSFileSystem) dir)
  "Раскрыть и сохранить содержимое каталога dir"
  (if (or (null (get-hash dir 'dir)) (not (equal (get-hash dir 'dir) t))) nil
      (let ((left-size (get-hash dir 'size))
            (blocks (get-hash dir 'blocks))
            (dir-list ())
            (block-stream nil))
	(while (and (not (null blocks)) (> left-size 0))
          (setq block-stream (stream-from-arr (block-read (car blocks)) nil))
          (while (and (not (null block-stream)) (> left-size 0))
            (let ((next-byte (get-byte block-stream)))
              (if (or (null next-byte) (= (car next-byte) 0)) (setq block-stream ()
								    left-size (- left-size (- *block-size* (AStream-byte-num block-stream))))
                  (progn (setq left-size (- left-size (car next-byte)))
                         (when (>= left-size 0)
                           (let ((parse-res (funcall (parse-cdfs-dir-entry) block-stream)))
                             (setq dir-list (append dir-list (list (car parse-res)))
                                   block-stream (cdr parse-res))))))))
          (setq blocks (cdr blocks)))
	(set-hash dir 'dir dir-list))))

(defmethod fstat ((self CDFSFileSystem) file-hash)
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
    (set-hash stat 'modify-date nil)
    (set-hash stat 'modify-time nil)
    (set-hash stat 'access-date nil)
    (set-hash stat 'access-time nil)
    (set-hash stat 'isdir (if (null (get-hash file-hash 'dir)) nil t))
    (set-hash stat 'flags (get-hash file-hash 'attributes))
    stat))

(defmethod open-file ((self CDFSFileSystem) file-hash)
  "Открыть файл как поток для чтения и записи"
  (when (not (null (get-hash file-hash 'dir))) (throw 'error "open-file: directories cannot be opened"))
  (let ((file (make-instance 'CDFSFile)))
    (CDFSFile-set-name file (get-hash file-hash 'name))
    (CDFSFile-set-size file (get-hash file-hash 'size))
    (CDFSFile-set-position file '(0 . 0))
    (CDFSFile-set-blocks file (get-hash file-hash 'blocks))
    (CDFSFile-set-dir file (if (null (get-hash file-hash 'dir)) nil t))
    file))
