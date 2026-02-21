(defconst +fat-block-free+ 0) ;; блок свободен
(defconst +fat-block-damaged+ 268435447) ;; 0xFFFFFF7 блок повреждён
(defconst +fat-block-end+ 268435455) ;; 0xFFFFFFF блок является концом цепочки
(defconst +fat-elem-size+ 4) ;; размер 1 элемента таблицы FAT в байтах
(defconst +fat-min-elem+ 2) ;; минимальный незарезервированный номер в таблице FAT

(defun fat-elem-pos (idx)
  "Получить пару из смещение сектора + смещение в секторе для элемента с индексом idx таблицы FAT"
  (cons (/ (* idx +fat-elem-size+) +sector-size+) (% (* idx +fat-elem-size+) +sector-size+)))

(defun get-fat-chain (block-num)
  "Получить цепочку блоков из таблицы FAT, начиная с блока block-num"
  (let ((fat-hash (FAT32FileSystem-fat-table *file-system*))
        (fat-table nil)
        (cur-elem nil)
        (prev-elem nil)
        (fat-chain nil))
    (when (>= block-num (/ (* (FAT32FileSystem-fat-sectors *file-system*) +sector-size+) +fat-elem-size+))
      (throw 'error "get-fat-chain: block-num out of range"))
    (if (check-key fat-hash block-num) (cons block-num (get-hash fat-hash block-num))
        (progn
          (setq fat-table (make-array (/ (* (FAT32FileSystem-fat-sectors *file-system*) +sector-size+) +fat-elem-size+)))
          (let ((cur-table-elem 0))
            (for i 0 (FAT32FileSystem-fat-sectors *file-system*)
                 (let ((sector-array (ata-read-sectors *disk* (+ (aref (FAT32FileSystem-fat-start-sectors *file-system*) (FAT32FileSystem-fat-active-num *file-system*)) i) 1))
                       (cur-pos 0))
                   (while (<= cur-pos (- +sector-size+ +fat-elem-size+))
                     (let ((fat-elem (arr-get-num sector-array cur-pos +fat-elem-size+)))
                       (seta fat-table cur-table-elem fat-elem)
                       (incf cur-table-elem)
                       (setq cur-pos (+ cur-pos +fat-elem-size+)))))))
          (setq prev-elem block-num)
          (setq cur-elem (aref fat-table block-num))
          (while (!= cur-elem +fat-block-end+)
            (setq fat-chain (append fat-chain (list prev-elem)))
            (seta fat-table prev-elem -1)
            (setq prev-elem cur-elem
                  cur-elem (aref fat-table cur-elem))
            (when (and (< cur-elem +fat-block-damaged+) (>= cur-elem (* (FAT32FileSystem-fat-sectors *file-system*) (/ +sector-size+ +fat-elem-size+))))
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

(defun get-free-block (fat-table free-block-num)
  "Найти первый свободный блок в таблице FAT fat-table, начиная поиск с free-block-num"
  (let ((free-num nil))
    (for i (+ free-block-num 1) (array-size fat-table)
         (when (= (aref fat-table i) +fat-block-free+)
           (setq free-num i)
           (setq i (array-size fat-table))))
    (when (null free-num)
      (for i +fat-min-elem+ (+ free-block-num 1)
           (when (= (aref fat-table i) +fat-block-free+)
             (setq free-num i)
             (setq i (+ free-block-num 1))))
      (when (null free-num) (throw 'error "get-free-block: free block not found")))
    free-num))

(defun update-fat-elem (idx val)
  "Установить элемент с индексом idx в таблице FAT на диске на значение val при необходимости обновить во всех копиях таблицы FAT"
  (let ((pos (fat-elem-pos idx))
        (fat-start-sectors (FAT32FileSystem-fat-start-sectors *file-system*))
        (fat-size (FAT32FileSystem-fat-sectors *file-system*))
        (fat-active-num (FAT32FileSystem-fat-active-num *file-system*))
        (fat-copying (FAT32FileSystem-fat-copying *file-system*)))
    (when (not fat-copying)
      (setq fat-start-sectors (array-seq fat-start-sectors fat-active-num (+ fat-active-num 1))))
    (for i 0 (array-size fat-start-sectors)
         (let* ((sector-num (+ (aref fat-start-sectors i) (car pos)))
                (sector-array (ata-read-sectors *disk* sector-num 1)))
           (arr-set-num sector-array (cdr pos) val +fat-elem-size+)
           (ata-write-sectors *disk* sector-num 1 sector-array)))))

(defun append-fat-chain (first-block new-idx)
  "Добавить элемент new-idx в цепочку блоков с первым блоком first-block"
  (let ((blocks (get-fat-chain first-block)))
    (update-fat-elem (last blocks) new-idx)
    (update-fat-elem new-idx +fat-block-end+)
    (setq blocks (append blocks (list new-idx)))
    (set-hash (FAT32FileSystem-fat-table *file-system*) (car blocks) (cdr blocks))))
