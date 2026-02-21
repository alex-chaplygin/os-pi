;; Класс абстрактной файловой системы
(defclass FileSystem () ())

;; (defmethod fs-init ((self FileSystem))
;;   "Загрузка файловой системы"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod create-dir ((self FileSystem))
;;   "Создать каталог"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod remove-dir ((self FileSystem))
;;   "Удалить каталог, если он пуст"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod load-dir ((self FileSystem))
;;   "Получить хеш-объект каталога"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod create-file ((self FileSystem))
;;   "Создать и открыть файл"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod remove-file ((self FileSystem))
;;   "Удалить файл"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod open-file ((self FileSystem))
;;   "Открыть файл как поток для чтения и записи"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod rename ((self FileSystem))
;;   "Переименовать файл или каталог"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod free-space ((self FileSystem))
;;   "Получить объём свободного места в текущем разделе диска"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; (defmethod fstat ((self FileSystem))
;;   "Получить метаданные файла или каталога"
;;   "Реализовать в конкретной файловой системе"
;;   nil)

;; Класс абстрактного файла
(defclass File ()
  (name size position blocks dir))

(defmethod is-directory ((self File))
  "Проверка на каталог"
  (File-dir self))

(defmethod close-file ((self File))
  "Закрытие файла"
  ;; (rplaca self nil)
  ;; (rplacd self nil)
  (for i 0 (array-size self)
       (seta self i 0)))

(defmethod tell-file ((self File))
  "Получение позиции в файле"
  (let ((pos (File-position self)))
    (get-offset-from-pos pos)))

(defmethod seek-file ((self File) offset origin)
  "Сместить позицию в файле на offset, начиная с origin"
  (let* ((pos (File-position self))
         (file-offset
          (case origin
            ('SET
             offset)
            ('CUR
             (+ offset (get-offset-from-pos pos)))
            ('END
             (+ offset (File-size self)))
            (otherwise
             (throw 'error "seek-file: unknown origin")))))
    (when (or (> file-offset (File-size self)) (< file-offset 0))
      (throw 'error "seek-file: position out of bounds of file"))
    (File-set-position self (get-blocks-pos (File-blocks self) file-offset))))

(defmethod read-file ((self File) size)
  "Прочитать size байт из файла и сместить позицию на тоже число"
  "Добавить изменения метаданных в конкретной файловой системе"
  (unless (integerp size)
    (throw 'error "read-file: size must be integer"))
  (when (< size 0)
    (throw 'error "read-file: size must be positive"))
  (when (> (+ (tell-file self) size) (File-size self))
    (throw 'error "read-file: size out of bounds of file"))
  (let ((pos (File-position self))
        (blocks (File-blocks self))
        (buf nil)
        (left-size size))
    (for i 0 (car pos)
         (setq blocks (cdr blocks)))
    (setq buf (array-seq (block-read (car blocks)) (cdr pos) *block-size*))
    (setq pos (cons (car pos) (+ (cdr pos) (array-size buf))))
    (setq blocks (cdr blocks))
    (setq left-size (- left-size (array-size buf)))
    (while (> left-size 0)
      (setq pos (cons (+ (car pos) 1) (cdr pos)))
      (setq buf (array-cat buf (block-read (car blocks))))
      (setq blocks (cdr blocks))
      (setq left-size (- left-size *block-size*)))
    (setq pos (cons (car pos) (+ (cdr pos) left-size)))
    (when (>= (cdr pos) *block-size*)
      (setq pos (cons (+ (car pos) 1) (- (cdr pos) *block-size*))))
    (File-set-position self pos)
    (array-seq buf 0 size)))

(defmethod write-file ((self File) buf)
  "Записать в файл массив байт buf, сместив позицию на соответствующее число"
  "Добавить изменения метаданных в конкретной файловой системе"
  "Добавить добавление новых блоков к файлу в конкретной файловой системе"
  ;; (unless (arrayp buf)
  ;;   (throw 'error "write-file: buf must be array"))
  (when (> (+ (tell-file self) (array-size buf)) (File-size self))
    (throw 'error "write-file: size out of bounds of file"))
  (let ((pos (File-position self))
        (blocks (File-blocks self))
        (left-size (array-size buf))
        (block-buf nil))
    (for i 0 (car pos)
         (setq blocks (cdr blocks)))
    (unless (= (cdr pos) 0)
      (setq block-buf (block-read (car blocks)))
      (setq block-buf (if (>= left-size (- *block-size* (cdr pos)))
                          (array-cat
                           (array-seq block-buf 0 (cdr pos))
                           (array-seq buf 0 (- *block-size* (cdr pos))))
                          (array-cat
                           (array-cat
                            (array-seq block-buf 0 (cdr pos))
                            (array-seq buf 0 left-size))
                           (array-seq block-buf (+ (cdr pos) left-size) *block-size*))))
      (block-write (car blocks) block-buf)
      (setq left-size (- left-size (- *block-size* (cdr pos))))
      (if (> left-size 0)
          (progn
            (setq blocks (cdr blocks))
            (setq pos (cons (+ (car pos) 1) 0))
            (setq buf (array-seq buf (- *block-size* (cdr pos)) left-size)))
          (setq pos (cons (car pos) (+ (cdr pos) (array-size buf))))))
    (while (>= left-size *block-size*)
      (block-write (car blocks) (array-seq buf 0 *block-size*))
      (setq blocks (cdr blocks))
      (setq buf (array-seq buf *block-size* left-size))
      (setq left-size (- left-size *block-size*))
      (setq pos (cons (+ (car pos) 1) (cdr pos))))
    (unless (<= left-size 0)
      (setq block-buf (block-read (car blocks)))
      (setq block-buf (array-cat
                       (array-seq buf 0 left-size)
                       (array-seq block-buf left-size *block-size*)))
      (block-write (car blocks) block-buf)
      (setq pos (cons (car pos) (+ (cdr pos) left-size))))
    (when (>= (cdr pos) *block-size*)
      (setq pos (cons (+ (car pos) 1) (- (cdr pos) *block-size*))))
    (File-set-position self pos)
    nil))
