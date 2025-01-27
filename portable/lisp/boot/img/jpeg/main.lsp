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
(defconst +huff-tables-num+ 4) ;Максимальное число таблиц Хаффмана
(defconst +block-size+ 64) ;Число байт в блоке JPEG
(defconst +quant-struct-size+ 67) ;64+2+1 Размер структуры таблицы квантования
(defconst +num-huff-code-len+ 16) ;Количество длин кодов Хаффмана

(defvar *quant-tables*) ;Массив таблиц квантования
(defvar *huff-tables-ac*) ;Массив таблиц AC Хаффмана
(defvar *huff-tables-ac-0*)
(defvar *huff-tables-ac-1*)
(defvar *huff-tables-dc*) ;Массив таблиц DC Хаффмана
(defvar *huff-tables-dc-0*)
(defvar *huff-tables-dc-1*)
(defvar *sample-precision*) ;Коэффициент сжатия
(defvar *num-lines*) ;Кол-во строк в изображении
(defvar *num-samples-per-line*) ;Кол-во элементов в строке изображения
(defvar *num-components*) ;Кол-во цветовых компонентов в изображении
(defvar *scan-cs*) ;Идентификаторы компонент скана
(defvar *scan-td*) ;Идентификаторы DC-таблиц Хаффмана
(defvar *scan-ta*) ;Идентификаторы AC-таблиц Хаффмана
(defvar *compoments-id*) ;Идентификаторы компонентов изображения
(defvar *compoments-h*) ;Горизонтальныe коэф компонентов
(defvar *compoments-v*) ;Вертикальныe коэф компонентов
(defvar *compoments-quant-table*) ;Номера таблиц квантования компонентов
(defvar *mcu*) ;Список с сохраненными MCU в виде массива YCbCr

(defun next-segment (ln struct-ln)
  "Пропустить байты сегмента, которые лежат за пределами структуры"
  (when (> ln struct-ln)
    (get-array (- ln struct-ln))))

(defun read-marker (marker)
  "Ожидание маркера marker"
  (if (= (eval marker) (get-word)) t (print `(,marker not found))))

(defun read-quant-table ()
  "Чтение сегмента таблицы квантования"
  (let ((tbl (get-struct '((ln . word) (tq . byte) (array . ,+block-size+)))))
    (seta *quant-tables* (get-hash tbl 'tq) (get-hash tbl 'array))
    (while (not (= 0xFF (get-next-byte)))
      (seta *quant-tables* (get-byte) (get-array +block-size+)))))

(defun arr-get-sum (arr size)
  "Вычисление суммы элементов массива arr длиной size"
  (let ((sum 0))
    (for i 0 size
	 (setq sum (+ sum (aref arr i))))
    sum))
	 
(defun make-len-arr (len)
  "Создать массив длин из массива по длинам len"
  (let ((ans (make-array (++ (arr-get-sum len +num-huff-code-len+))))
	(count 0))
    (for i 0 +num-huff-code-len+
	 (let ((temp (aref len i)))
	   (when (not (= 0 temp))
	     (for j 0 temp
		  (seta ans count (++ i))
		  (setq count (++ count))))))
    (seta ans count 0)
    ans))
	   
(defun get-huff-table (len)
  "Создание и заполнение таблицы Хаффмана по массиву длин кодов len"
  (let ((huff (make-huff))
	(c 0)
	(s 1)
	(k 0)
	(not-end t))
    (while not-end
      (while (= s (aref len k))
	(setq huff (huff-add huff c s (get-byte)))
	(setq c (++ c))
	(setq k (++ k)))
      (if (not (= 0 (aref len k)))
	  (progn
	    (setq c (<< c 1))
	    (setq s (++ s))
	    (while (not (= s (aref len k)))
	      (setq c (<< c 1))
	      (setq s (++ s))))
	  (setq not-end nil)))
      huff))

(defun read-huff-table()
  "Чтение и восстановление таблицы Хаффмана"
  (let ((tbl (get-struct '((lh . word) (TcTh . bits4) (l . ,+num-huff-code-len+))))
	(temp nil)
	(marker nil))
    (print (get-hash tbl 'tcth))
    (setq marker (get-hash tbl 'tcth))
    (setq temp (get-huff-table (make-len-arr (get-hash tbl 'l))))
    ;(print temp)
    (if (= 0 (car marker))
	(progn
	  (seta *huff-tables-dc* (cdr marker) temp)
	  (print `(dc ,(cdr marker)))
	  (print (aref *huff-tables-dc* (cdr marker)))
	  (if (= 0 (cdr marker))
	      (setq *huff-tables-dc-0* temp)
	      (setq *huff-tables-dc-1* temp)))
	(progn
	  (seta *huff-tables-ac* (cdr marker) temp)
	  (print `(ac ,(cdr marker)))
	  (print (aref *huff-tables-ac* (cdr marker)))
	  (if (= 0 (cdr marker))
	      (setq *huff-tables-ac-0* temp)
	      (setq *huff-tables-ac-1* temp))))
    t))

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
  "Чтение таблиц квантования и Хаффмана"
  (let* ((marker (get-word))
	 (val (case marker
		(DQT (progn (print "DQT") (read-quant-table)))
		(DHT (progn (print "DHT") (read-huff-table)))
		(DRI (progn (print "DRI") (read-restart-interval)))
		(COM (progn (print "COM") (read-comment)))
		(APP (progn (print "APP") (read-app)))
		(otherwise nil))))
    (print marker)
    (unless (null val) (read-tables))))

(defun read-frame-header ()
  "Прочесть заголовок кадра"
  (let ((frm (get-struct '((lf . word) (p . byte) (y . word) (x . word) (nf . byte)))))
    (print "SOF")
    (print frm)
    (setq *sample-precision* (get-hash frm 'p))
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

(defun read-scan-header ()
  "Прочесть заголовок скана"
  (let* ((tbl (get-struct '((ls . word) (ns . byte))))
	 (temp (get-hash tbl 'ns)))
    (print "SOS")
    (print "scan-header")
    (setq *scan-cs* (make-array temp))
    (setq *scan-td* (make-array temp))
    (setq *scan-ta* (make-array temp))
    (for i 0 (get-hash tbl 'ns)
	 (seta *scan-cs* i (get-byte))
	 (setq temp (get-4bit))
	 (seta *scan-td* i (car temp))
	 (seta *scan-ta* i (cdr temp)))
    (set-hash tbl 'ss (get-byte))
    (set-hash tbl 'se (get-byte))
    (set-hash tbl 'ahal (get-4bit))))

(defun decode-mcu ()
  "Чтение одного MCU"
  (let ((mcuArr nil);Массив с data-unit
	(numDU (make-array *num-components*)));Массив с количеством data-unit для каждого компонента
    (for i 0 *num-components*
	 (seta numDU i (* (aref *compoments-h*
				(-- (aref *compoments-id* i)))
			  (aref *compoments-v*
				(-- (aref *compoments-id* i))))))
    (let* ((sumDU (arr-get-sum numDU *num-components*));Всего data-unit в MCU
	   (count 0));Счетчик data-unit
      (setq mcuArr (make-array sumDU))
      (for i 0 *num-components*
	   (for j 0 (aref numDU i)
		(seta mcuArr count
		      (dequant (aref *quant-tables* (aref *compoments-quant-table* i))
			 (decode-data-unit i (aref *huff-tables-dc* (aref *scan-td* i))
					   (aref *huff-tables-ac* (aref *scan-ta* i)))))
		(setq count (++ count))))
      (setq *mcu* (list (append *mcu* mcuArr))))))

(defun scan ()
  "Чтение сегмента скан"
  (read-tables)
  (read-scan-header)
  (print `(scan-cs ,*scan-cs*))
  (print `(scan-td ,*scan-td*))
  (print `(scan-ta ,*scan-ta*))
  (print "data-unit")
  ;;(print (aref *quant-tables* 0))
  (setq *pred* (make-array *num-components*))
  (for i 0 *num-components*
       (seta *pred* i 0))
  (print (decode-mcu))
  (print (decode-mcu))
  (print (decode-mcu))
  ;;(print *pred*)
  ;;(print (dequant (aref *quant-tables* 0)
;;		  (decode-data-unit 0 (aref *huff-tables-dc* (aref *scan-td* 0))
;;				    (aref *huff-tables-ac* (aref *scan-ta* 0)))))
  )

(defun read-frame ()
  "Чтение кадра"
  (read-tables)
  (print `(quant-tables ,*quant-tables*))
  (get-word)
  (read-frame-header)
  (print `(sample-precision ,*sample-precision*))
  (print `(y ,*num-lines*))
  (print `(x ,*num-samples-per-line*))
  (print `(components ,*num-components*))
  (print `(id ,*compoments-id*))
  (print `(h ,*compoments-h*))
  (print `(v ,*compoments-v*))
  (print `(quant-table-selector ,*compoments-quant-table*))
  (scan)
  )

(defun read-jpeg (image)
  "Декодирует изображение JPEG"
  "image - массив байт закодированного изображения"
  "Возвращает матрицу декодированного изображения"  
  (set-bin-src *image*)
  (set-big-endian)
  (setq *quant-tables* (make-array +quant-tables-num+))
  (setq *huff-tables-ac* (make-array +huff-tables-num+))
  (setq *huff-tables-dc* (make-array +huff-tables-num+))
  (read-marker 'SOI)
  (read-frame)
  (read-marker 'EOI))
