(defconst SOI  0xFFD8) ;Начало изображения
(defconst EOI  0xFFD9) ;Конец изображения
(defconst SOS  0xFFDA) ;Начало скана
(defconst DQT  0xFFDB) ;Определитель таблицу квантизации
(defconst DNL  0xFFDC) ;Определитель кол-ва строк 
(defconst DRI  0xFFDD) ;Определитель интервала перезапуска
(defconst DHP  0xFFDE) ;Определитель иерархической прогрессии
(defconst APP  0xFFE0) ;Зарезервировано для сегментов приложенй
(defconst JPG  0xFFF0) ;Зарезервировано для расширений JPEG
(defconst COM  0xFFFE) ;Комментарий
(defconst SOF  0xFFC0) ;Начало основного кадра JPEG
(defconst DHT  0xFFC4) ;Определитель таблицы Хаффмана
(defconst RST  0xFFD0) ;Интервал перезапуска параметров

(defconst +quant-tables-num+ 4) ;Максимальное число таблиц квантования
(defconst +block-size+ 64) ;Число байт в блоке JPEG
(defconst +quant-struct-size+ 67) ;64+2+1 Размер структуры таблицы квантования
(defvar *quant-tables*) ;Массив таблиц квантования
(defvar *num-lines*) ;Кол-во строк в изображении
(defvar *num-samples-per-line*) ;Кол-во элементов в строке изображения
(defvar *num-components*) ;Кол-во цветовых компонентов в изображении
(defvar *compoments-id*) ;Идентификаторы компонентов изображения
(defvar *compoments-h*) ;Горизонтальныe коэф компонентов
(defvar *compoments-v*) ;Вертикальныe коэф компонентов
(defvar *compoments-quant-table*) ;Номера таблиц квантования компонентов

(defun next-segment (ln struct-ln)
  "Пропустить байты сегмента, которые лежат за пределами структуры"
  (when (> ln struct-ln)
    (get-array (- ln struct-ln))))

(defun read-marker (marker)
  "Ожидание маркера marker"
  (if (= (eval marker) (get-word)) t (error `(,marker not found))))

(defun read-quant-table ()
  "Чтение сегмента таблицы квантования"
  (let ((tbl (get-struct `((ln . word) (tq . byte) (array . ,+block-size+)))))
    (next-segment (get-hash tbl 'ln) +quant-struct-size+)
    (seta *quant-tables* (get-hash tbl 'tq) (get-hash tbl 'array))))

(defun read-huff-table()
(print "read-huff-table")
  t)

(defun read-restart-interval()
(print "read-restart-table")
  t)

(defun read-comment()
(print "read-comment")
  t)

(defun read-app()
  "Читает и возвращает сегмент данных приложения"
  (let ((ln (get-word)))
    (get-array (- ln 2))))

(defun read-tables ()
  "Чтение таблиц"
  (let ((marker (get-word)))
    (print marker)
    (while (not (null (case marker
	      (DQT (read-quant-table))
	      (DHT (read-huff-table))
	      (DRI (read-restart-interval))
	      (COM (read-comment))
	      (APP (read-app))
	      (otherwise nil))))
      (read-tables))))

(defun read-frame-header ()
  "Прочесть заголовок кадра"
  (let ((frm (get-struct '((lf . word) (p . byte) (y . word) (x . word) (nf . byte)))))
    (setq *num-lines* (get-hash frm 'y))
    (setq *num-samples-per-line* (get-hash frm 'x))
    (setq *num-components* (get-hash frm 'nf))
    (setq *compoments-id* (make-array *num-components*))
    (setq *compoments-h* (make-array *num-components*))
    (setq *compoments-v* (make-array *num-components*))
    (setq *compoments-quant-table* (make-array *num-components*))
    (for i 0 *num-components*
	 (let ((com (get-struct '((c . byte) (hv . bits4) (tq . byte)))))
	   (seta *compoments-id* i (get-hash com 'c))
	   (seta *compoments-h* i (car (get-hash com 'hv)))
	   (seta *compoments-v* i (cdr (get-hash com 'hv)))
	   (seta *compoments-quant-table* i (get-hash com 'tq))))))

(defun read-frame ()
  "Чтение кадра"
  (read-tables)
  (read-frame-header)
  (read-tables)
  )

(defun read-jpeg (image)
  "Декодирует изображение JPEG"
  "image - массив байт закодированного изображения"
  "Возвращает матрицу декодированного изображения"  
  (set-bin-src *image*)
  (set-big-endian)
  (setq *quant-tables* (make-array +quant-tables-num+)) 
  (read-marker 'SOI)
  (read-frame)
  (read-marker 'EOI))
