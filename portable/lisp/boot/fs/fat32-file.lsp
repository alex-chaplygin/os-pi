(defmethod close-file ((self FAT32File))
  "Закрыть файл"
  (super close-file self))

(defmethod tell-file ((self FAT32File))
  "Получить позицию внутри потока файла"
  (super tell-file self))

(defmethod seek-file ((self FAT32File) offset origin)
  "Сместить позицию в файле на offset, начиная с origin(SET(начало) CUR(текущее) END(конец))"
  (super seek-file self offset origin))

(defmethod read-file ((self FAT32File) size)
  "Прочить size байт из файла и сместить позицию на тоже число. Изменить время последнего доступа"
  (let ((read-buf (super read-file self size))
        (time-list (get-cur-time))
        (access-date nil))
    (setq access-date (list (nth time-list 0) (nth time-list 1) (nth time-list 2)))
    (fat32-update-entry (FAT32File-dir-entry self) `((access-date . ,access-date)))
    read-buf))

(defmethod write-file ((self FAT32File) buf)
  "Записать в файл массив байт buf, сместив позицию на соответствующее число"
  "Изменить время последнего доступа, время последнего изменения"
  "При необходимости выделить новый блок"
  (when (contains (get-hash (FAT32File-dir-entry self) 'attributes) 'READ-ONLY) (raise 'read-only "write-file: file is read-only"))
  (let ((left-size (- (+ (tell-file self) (array-size buf)) (* *block-size* (list-length (FAT32File-blocks self)))))
        (time-list (get-cur-time))
        (access-date nil)
        (modify-date-time nil)
        (new-size (FAT32File-size self)))
    (when (> (+ (tell-file self) (array-size buf)) (FAT32File-size self))
      (setq new-size (+ (tell-file self) (array-size buf)))
      (FAT32File-set-size self new-size))
    (while (> left-size 0)
      (new-block *file-system* (FAT32File-dir-entry self))
      (setq left-size (- left-size *block-size*)))
    (FAT32File-set-blocks self (get-fat-chain (get-hash (FAT32File-dir-entry self) 'first-block)))
    (super write-file self buf)
    (setq access-date (list (nth time-list 0) (nth time-list 1) (nth time-list 2)))
    (setq modify-date-time (list (nth time-list 0) (nth time-list 1) (nth time-list 2) (nth time-list 3) (nth time-list 4) (nth time-list 5)))
    (fat32-update-entry (FAT32File-dir-entry self) `((access-date . ,access-date) (modify-date-time . ,modify-date-time) (size . ,new-size)))))
