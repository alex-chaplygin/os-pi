(defun fs-init (fs disk start-sector sector-count)
  "Заглушка для функции fs-init"
  (list disk start-sector sector-count))

;; Заглушки для классов
(defclass FAT32FileSystem () ())
(defclass CDFSFileSystem () ())

(defvar *file-system* nil)
