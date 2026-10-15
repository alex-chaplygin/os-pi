(defconst +TTF-SIGNATURE1+ #(0x00 0x01 0x00 0x00))
(defconst +TTF-SIGNATURE2+ #(0x74 0x72 0x75 0x65))

(defun make-ttf-sig (str)
  "Преобразование теги ttf в массив байт"
  (list-to-array (map #'char-code (explode str))))

(defconst +CMAP+ (make-ttf-sig "cmap")) ; Таблица отображения символов на глифы (кодировка)
(defconst +HEAD+ (make-ttf-sig "head")) ; Заголовок шрифта
(defconst +HHEA+ (make-ttf-sig "hhea")) ; Заголовок горизонтального выравнивания (метрик)
(defconst +MAXP+ (make-ttf-sig "maxp")) ; Таблица максимальных параметров (профиль ограничений)
(defconst +NAME+ (make-ttf-sig "name")) ; Таблица имен и метаданных (название, копирайт и др.)
(defconst +OS2+ (make-ttf-sig "os2 ")) ; Специфичные метрики для OS/2 и Windows
(defconst +POST+ (make-ttf-sig "post")) ; Информация для PostScript-принтеров и интерпретаторов

(defun ttf-signature ()
  "Ожидает сигнатуру ttf"
  (parse-or (parse-elem-array +TTF-SIGNATURE1+) (parse-elem-array +TTF-SIGNATURE2+)))

(defun ttf-table-tag ()
  "Тег - тип таблицы, 4 символа, преобразуем в строку"
  (parse-app (parse-array 4) #'(lambda (a) (arr-get-str a 0 4))))

(defun ttf-cmap ()
  "Разбор таблицы отображения символов на глифы (кодировка)"
  (parse-suc 'CMAP))

(defun ttf-table (sig parser)
  "Разбор таблицы с сигнатурой sig. Выполняет перемотку потока на нужную позицию, и вызывает разбор таблицы parser"
  (&&& (parse-elem-array sig)
       checksum-> #'get-dword
       offset-> #'get-dword
       length-> #'get-dword
       table-> #'(lambda (s)
		   (let* ((res (funcall parser (stream-seek s offset 'seek-set))))
		     (if res (cons (car res) s) (error "Cannot parse table" (arr-get-str a 0 4)))))
       return table))

(defun ttf-table-rec ()
  "Запись таблицы"
  (&&& (ttf-table-tag) #'get-dword #'get-dword #'get-dword))

(defun ttf ()
  "Файл TTF"
  (&&& (ttf-signature)
       num-tables-> #'get-word
       #'get-word #'get-word #'get-word
       tables-> (parse-many-n num-tables (parse-or (ttf-table +CMAP+ (ttf-cmap))
						   (ttf-table-rec)))
       return tables))


(defun load-font (font-data)
  "Загрузить шрифт из массива байт font-data"
  (let ((f (funcall (ttf) (stream-from-arr font-data t))))
    (if f (car f) "Ошибка загрузки шрифта")))
