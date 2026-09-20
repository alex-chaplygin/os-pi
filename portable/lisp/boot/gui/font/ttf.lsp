(defun ttf-table-rec ()
  "Запись таблицы"
  (&&& (parse-array 4) #'get-dword #'get-dword #'get-dword))

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
