					; Структура системы:
					; LBA
					; 0 - Bios Parameter Block (BPB)
					; 1 - FSInfo
					; 2 - Резерв
					; ...
					; 7 - Копия BPB
					; ...
					; xx - FAT1
					; адрес - BPB->ReservSecCount
					; длина - BPB->FATSize32
					; xx - FAT2 (копия)
					; xx - Данные
					; xx - Корневой каталог
					; маркер 0xF8FFFF0F.FFFFFFFF
					; Данные
(defvar bios-parameter-block '(
(jmpboot         .3     ) ;// JMP на загрузчик (0xEB5890)
(oemname         .8     ) ;// <----- строка форматера ОС
(bytepersector   .2     ) ;// Байт в секторе
(secpercluster   .1     ) ;// Секторов к кластере
(rsvdseccounter  .2     ) ;// Резервная область в секторах
(numfats         .1     ) ;// Сколько копий FAT-таблицы
(rootentcnt      .2     ) ;// Объектов в корневом каталоге (нуль для FAT-32)
(totsec16        .2     ) ;// Всего секторов на диске (нуль для FAT-32)
(media           .1     ) ;// Тип диска (F8)
(fatsz16         .2     ) ;// Размер таблицы FAT-16 в секторах (нуль для FAT-32)
(secpertrk       .2     ) ;// Секторов в дорожке
(numheads        .2     ) ;// Всего головок Head
(hiddsec         .4     ) ;// Cекторов перед началом раздела
(totsec32        .4     ) ;// Всего секторов на диске
(fatsz32         .4     ) ;// Размер таблицы FAT-32 в секторах
(extflags        .2     ) ;// Флаги файловой системы(Номер активной ФС,зеркалирована ли )
(fsver           .2     ) ;// Версия ФС
(rootclus        .4     ) ;// Номер корневого кластера(обычно 2)
(fsinfo          .2     ) ;// Сектор структуры FSinfo
))

(defvar *fs-info-sec*) ; сектор структуры FSInfo

; класс для системы FAT32
(defclass Fat32FileSystem FileSystem ())

; класс для файла FAT32
(defclass Fat32File File
  (dname ; имя в структуре записи каталога
   block-hi ; номер первого блока в записи каталога
   block-low
   dir-block ; номер блока каталога
   dir-offset ; смещение в блоке каталога;
   attributes ; атрибуты;
   start-block ; номер первого блока в цепочке;
   ctime  ;время создания файла
   cdate  ;дата создания файла
   adate  ;дата последнего обращения к файлу
   wdate  ;дата последней записи файла
   wtime))  ;время последней записи файла

(defmethod init ((self Fat32FileSystem) disk start end)
  "Инициализация ФС на диске disk, начиная с сектора start, заканчивая end"
  (setq *disk* disk)
  (let ((sec (block-read start))
	(fs (block-read (+ start 1))))
    (with-struct bios-parameter-block sec 0
      (setq
       *block-sectors* secpercluster
       *block-size* (* bytepersector *block-sectors*)
       *fat-start-sector* (+ start rsvdseccounter)
       *fat-sectors* fatsz32
       *block-sector-offset* (+ *fat-start-sector*
				(* *fat-sectors* numfats)
				(- 0 secpercluster secpercluster))
       *root-block* (+ start rootclus)))
    (setq *free-blocks-count* (arr-get-num fs +free-blocks-count-pos+ 4)
	  *last-free-block* (arr-get-num fs +last-free-block-pos+ 4)
	  *fs-info-sec* (++ start)
	  *fat* (make-hash)
	  *root-directory* (load-dir self (get-fat-chain *root-block*))
	  *working-directory* *root-directory*)))

(defmethod load-dir((self Fat32FileSystem) block-list)
  "Загрузить каталог, находящийся в блоках из списка block-list"
  (let ((h (make-hash)))
    (app '(lambda (f) (set-hash h (slot f 'name) f)) (load-dir* block-list))
    h))

(defun load-dir*(block-list)
  "Загрузить каталог, находящийся в блоках из списка block-list"
  (if (null block-list) nil
      (append (make-dir (car block-list)) (load-dir* (cdr block-list)))))

(defmethod is-directory((self Fat32File))
  "Предикат: является ли файловый объект каталогом"
  (not (= (& (slot self 'attributes) +directory+) 0)))

(defmethod get-blocks ((self Fat32File))
  "Получить список блоков файла"
  (get-fat-chain (slot self 'start-block)))

(defmethod new-block ((self Fat32File))
  "Добавить новый блок к файлу, обновить FAT"
  (when (= *free-blocks-count* 0) (error "No free space"))
  (let ((bl (fat-get-free-block))
	(sb (slot self 'start-block)))
    (fat-append-chain sb bl)
    (when (null sb) ; новый файл
      (setf (slot self 'start-block) bl)
      (update-dir-entry self))
    (setf (slot self 'blocks) (get-fat-chain sb))))
