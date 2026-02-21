;; Класс файловой системы FAT32
(defclass FAT32FileSystem FileSystem (fat-table fat-start-sectors fat-sectors fat-active-num fat-copying fsinfo-sector free-blocks-count free-block-num volume-size root-entry))

;; Класс файла FAT32
(defclass FAT32File File (read-only dir-entry))

;; Заглушка переменной экземпляра класса файловой системы из модуля виртуальной файловой системы
(defvar *file-system* nil)
