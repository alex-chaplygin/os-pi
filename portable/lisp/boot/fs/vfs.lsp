(defvar *file-system* nil) ;; объект текущей файловой системы
(defvar *root-directory* nil) ;; файловый объект корневого каталога
(defvar *working-directory* nil) ;; файловый объект текущего каталога
(defvar *working-path* ()) ;; список пути к текущему каталогу

;; Вспомогательные функции

;; split (delimeter string)

(defun butlast (list)
  "Вернуть список без последнего элемента"
  (labels ((butlast* (new-list old-list)
             (unless (pairp (cdr old-list)) (raise 'error "butlast: not a list"))
             (if (null (cdr old-list))
                 new-list
                 (butlast* (append new-list (list (car old-list))) (cdr old-list)))))
    (unless (pairp list) (raise 'error "butlast: not a list"))
    (when (null list) (raise 'error "butlast: empty list"))
    (butlast* () list)))

(defun find-entry (dir name)
  "Найти файл или каталог с названием name в каталоге dir"
  (unless (pairp (get-hash dir 'dir)) (raise 'error "find-entry: dir is not a directory or dir is not loaded"))
  (let ((entries (car (get-hash dir 'dir)))
        (found nil))
    (while (not (null entries))
      (when (equal name (get-hash (car entries) 'name))
        (setq found (car entries)))
      (setq entries (cdr entries)))
    found))

(defun parse-up-dir (path-list)
  "Преобразовать список пути path-list убрав все .."
  (let ((last-working-idx 0)
        (working-up-dir nil)
        (is-working-path nil))
    (when (equal (car path-list) "working")
      (setq path-list (append *working-path* (cdr path-list)))
      (setq last-working-idx (-- (list-length *working-path*)))
      (setq is-working-path t))
    (let ((path-array (list-to-array path-list)))
      (for i (+ 1 last-working-idx) (array-size path-array)
           (when (equal (aref path-array i) "..")
             (seta path-array i nil)
             (let ((del-i (- i 1)))
               (while (null (aref path-array del-i)) (setq del-i (- del-i 1)))
               (when (= del-i 0) (raise 'path-not-found "parse-up-dir: path trying to up dir from root"))
               (when (<= del-i last-working-idx) (setq working-up-dir t))
               (seta path-array del-i nil))))
      (unless working-up-dir
        (setq path-array (array-cat #(nil) (array-seq path-array (+ 1 last-working-idx) (array-size path-array))))
        (seta path-array 0 (if is-working-path "working" "root")))
      (setq path-list nil)
      (let ((i (- (array-size path-array) 1)))
        (while (>= i 0)
          (when (aref path-array i)
            (setq path-list (cons (aref path-array i) path-list)))
          (decf i)))
      path-list)))

(defun parse-path (str-path)
  "Преобразовать строку пути str-path в список пути"
  (if (equal str-path "")
      '("working")
      (if (equal str-path "/")
          '("root")
          (progn
            (if (equal (char str-path 0) #\/)
		(setq str-path (concat "root" str-path))
		(setq str-path (concat "working/" str-path)))
            (let ((path-list (split #\/ str-path)))
              (parse-up-dir path-list))))))

(defun join (del str-list)
  "Конкатинации строк из str-list с разделителем del"
  (unless (pairp str-list) (raise 'error "join: str-list is not a list"))
  (when (null str-list) (raise 'error "join: str-list is empty"))
  (let ((str (car str-list)))
    (setq str-list (cdr str-list))
    (while (not (null str-list))
      (setq str (concat (concat str (make-string 1 del)) (car str-list)))
      (setq str-list (cdr str-list)))
    str))

(defun load-path (path)
  "Проверяет и при необходимости загружает путь path"
  "Возвращает файловый объект файла или каталога или вызывает ошибку если пути нет"
  (let* ((path-list (parse-path path))
         (path-dir (case (car path-list)
                     ("root" *root-directory*)
                     ("working" *working-directory*))))
    (setq path-list (cdr path-list))
    (when (equal path-list '("")) (setq path-list nil))
    (while (not (or (null path-list) (null path-dir)))
      (load-dir *file-system* path-dir)
      (unless (get-hash path-dir 'dir) (raise 'path-not-found "load-path: file in the middle of the path"))
      (setq path-dir (find-entry path-dir (car path-list)))
      (setq path-list (cdr path-list)))
    (unless path-dir (raise 'path-not-found "load-path: path not found"))
    (load-dir *file-system* path-dir)
    path-dir))

(defun get-parent-dir (path)
  "Получить файловый объект родительского каталога файла или каталога по строке пути path"
  (if (equal path "")
      (load-path "..")
      (let ((i (-- (string-size path)))
            (found nil))
        (while (>= i 0)
          (when (equal (char path i) #\/)
            (setq found (subseq path 0 i))
            (setq i -1))
          (decf i))
        (if found
            (load-path found)
            (load-path "")))))

;; Функции виртуальной файловой системы

;; load-partition (disk-num part-num)

(defun cur-dir ()
  "Получить путь текущего каталога"
  (let ((path (join #\/ (append '("") (cdr *working-path*)))))
    (if (equal path "") "/" path)))

(defun list-dir (path)
  "Получить содержимое каталога по пути path"
  (let ((entry (load-path path)))
    (if (null (get-hash entry 'dir))
        (raise 'not-directory "list-dir: not directory")
        (map #'(lambda (elem) (get-hash elem 'name)) (car (get-hash entry 'dir))))))

(defun create-dir (path)
  "Создать каталог по пути path где последний элемент пути это название нового каталога"
  (let* ((dir-path (butlast (split #\/ path)))
         (dir-entry (load-path (if (null dir-path) "" (join #\/ dir-path))))
         (name (last (split #\/ path))))
    (if (null (get-hash dir-entry 'dir))
        (raise 'not-directory "create-dir: not directory")
        (create-dir* *file-system* dir-entry name))))

(defun remove-dir (path)
  "Удалить каталог по пути path если он пуст"
  (let ((entry (load-path path))
        (parent-dir (get-parent-dir path)))
    (if (null (get-hash entry 'dir))
        (raise 'not-directory "remove-dir: not directory")
        (remove-dir* *file-system* parent-dir entry))))

(defun change-dir (path)
  "Переместиться в каталог по пути path"
  (let ((entry (load-path path))
        (new-path (parse-path path)))
    (if (null (get-hash entry 'dir))
        (raise 'not-directory "change-dir: not directory")
        (progn (setq *working-directory* entry)
               (setq *working-path*
                     (case (car new-path)
                       ("working" (append *working-path* (cdr new-path)))
                       ("root" new-path)
                       (otherwise (raise 'argument-error "change-dir: path origin error"))))))))

(defun create-file (path)
  "Создать файл по пути path где последний элемент пути это название нового файла"
  (let* ((dir-path (butlast (split #\/ path)))
         (dir-entry (load-path (if (null dir-path) "" (join #\/ dir-path))))
         (name (last (split #\/ path))))
    (if (null (get-hash dir-entry 'dir))
        (raise 'not-directory "create-file: not directory")
        (create-file* *file-system* dir-entry name))))

(defun remove-file (path)
  "Удалить файл по пути path"
  (let ((entry (load-path path))
        (parent-dir (get-parent-dir path)))
    (if (not (null (get-hash entry 'dir)))
        (raise 'not-file "remove-file: not file")
        (remove-file* *file-system* parent-dir entry))))

(defun open-file (path)
  "Открыть файл как поток для чтения и записи по пути path и вернуть файловый объект"
  (let ((entry (load-path path)))
    (if (not (null (get-hash entry 'dir)))
        (raise 'not-file "open-file: not file")
        (open-file* *file-system* entry))))

(defun is-directory (path)
  "Проверить является ли файл по пути path каталогом"
  (let ((entry (load-path path)))
    (not (null (get-hash entry 'dir)))))

(defun rename (path name)
  "Переименовать файл или каталог по пути path на name"
  (let ((entry (load-path path)))
    (rename* *file-system* entry name)))

(defun free-space ()
  "Получить объём свободного места в разделе"
  (free-space* *file-system*))

(defun fstat (path)
  "Получить метаданные файла или каталога"
  (let ((entry (load-path path)))
    (fstat* *file-system* entry)))

(defun set-attr (path new-attributes)
  "Изменить атрибуты файла или каталога по пути path на new-attributes"
  (let ((entry (load-path path)))
    (set-attr* *file-system* entry new-attributes)))

;; Функции файла при открытии через open-file

;; close-file (file) закрыть файл
;; read-file (file size) прочитать определённое количество байт из потока файла, и переместить позицию внутри потока на то же количество
;; write-file (file buffer) записать массив байт в поток файла, и переместить позицию внутри потока на то же количество
;; tell-file (file) получить позицию внутри потока файла
;; seek-file (file offset origin) переместить позицию внутри потока файла. origin - SET(начало) CUR(текущее) END(конец)

;; Возможные исключения при работе с виртуальной файловой системой

;; path-not-found путь не найден
;; not-directory по пути найден файл а не каталог
;; not-file по пути найден каталог а не файл
;; read-only файл только для чтения
;; not-enough-space не хватает места на диске
;; end-of-file чтение за пределами файла
;; not-empty-dir попытка удаления не пустого каталога
;; argument-error некорректные аргументы
;; file-exists файл или каталог уже существует
