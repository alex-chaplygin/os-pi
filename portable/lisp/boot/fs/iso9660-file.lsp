(defclass CDFSFile File ())

(defmethod is-directory ((self CDFSFile))
  "Проверка на каталог"
  (super is-directory self))

(defmethod close-file ((self CDFSFile))
  "Закрытие файла"
  (super close-file self))

(defmethod tell-file ((self CDFSFile))
  "Получение позиции в файле"
  (super tell-file self))

(defmethod seek-file ((self CDFSFile) offset origin)
  "Сместить позицию в файле на offset, начиная с origin(SET(начало) CUR(текущее) END(конец))"
  (super seek-file self offset origin))

(defmethod read-file ((self CDFSFile) size)
  "Прочить size байт из файла и сместить позицию на тоже число"
  (super read-file self size))

(defmethod write-file ((self CDFSFile) buf)
  "Вызов ошибки при попытке записи в файл в файловой системы CDFS(ISO9660)"
  (throw 'error "write-file: CDFS(ISO9660) is read-only"))
