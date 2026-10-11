(defun ttf-table-tag ()
  "Тег - тип таблицы, 4 символа, преобразуем в строку"
  (parse-app (parse-array 4) #'(lambda (a) (arr-get-str a 0 4))))

(defun ttf-table-rec ()
  "Запись таблицы"
  (&&& (ttf-table-tag) #'get-dword #'get-dword #'get-dword))

(defun ttf ()
  "Файл TTF"
  (&&& #'get-dword
       num-tables-> #'get-word
       #'get-word #'get-word #'get-word
       (parse-many-n num-tables (ttf-table-rec))))


(defun load-font (font-data)
  "Загрузить шрифт из массива байт font-data"
  (let ((f (funcall (ttf) (stream-from-arr font-data t))))
    (if f (car f) "Ошибка загрузки шрифта")))
