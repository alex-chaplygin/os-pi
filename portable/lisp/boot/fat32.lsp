					; Модуль для работы с FAT32
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
(defvar *disk*) ; номер диска, где находится FAT32
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
(extflags        .4     ) ;// Флаги файловой системы(Номер активной ФС,зеркалирована ли )
(fsver           .2     ) ;// Версия ФС
(rootclus        .4     ) ;// Номер корневого кластера(обычно 2)
(fsinfo          .2     ) ;// Сектор структуры FSinfo
(bkbootsec       .2     ) ;// Запасной загрузочный сектор
(reserved        .12    ) ;// Зарезервировано
(drvnum          .1     ) ;// Номер диска
(reserved2       .1     ) ;// Зарезервировано
(bootsig         .1     ) ;// Метка загрузочного диска
(volid           .4     ) ;// Идентификатор тома
(filsystype      .8     ) ;// Строка "FAT32   "
))

(defvar *fat*)
(defvar *dirinfo*)
(defvar *byte-per-sector*)
(defvar *cluster-size*)
(defvar *byte-per-sector*)
(defvar +fat32-sectors+)
(defvar +fsinfo-sect+)
(defvar fat32-dir-struct '((dir-name . 11)
                            (dir-attr . 1)
                            (dir-skip0 . 1)
                            (dir-created-time10 . 1)
                            (dir-created-time . 2)
                            (dir-created-date . 2)
                            (dir-last-access-date . 2)
                            (dir-cluster-high . 2)
                            (dir-write-time . 2)
                            (dir-write-date . 2)
                            (dir-cluster-low . 2)
                            (dir-size-bytes . 4)))

(defun load-fat32 (disk-num start num)
  "Загрузить файловую систему FAT32 диска disk-num секторы, начиная со start, число секторов - num"
  (setq *disk* disk-num)
  (let ((sec (ata-read-sectors *disk* start 1)))
    (with-struct bios-parameter-block sec 0
		 (setq
		  *byte-per-sector* bytepersector
		  *cluster-size* secpercluster)
		 (make-fat rsvdseccounter fatsz32))))

(defun make-fat (rescount size)
  "Прочитать FAT и преобразовать в массив int"
  (let ((fat (ata-read-sectors *disk* rescount size))
	(fat-count (/(* *byte-per-sector* size) 4)))
    (setq *fat* (make-array fat-count))
    (for i 0 fat-count
	 (seta *fat* i (arr-get-num fat (* 4 i) 4)))))

(load-partition 0 0)

