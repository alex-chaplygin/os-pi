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
))

(defun load-fat32 (disk-num start num)
  "Загрузить файловую систему FAT32 диска disk-num секторы, начиная со start, число секторов - num"
  (setq *disk* disk-num)
  (let ((sec (ata-read-sectors *disk* start 1)))
    (with-struct bios-parameter-block sec 0
      `((jmpboot         ,jmpboot        )
	(oemname       	 ,oemname        )
	(bytepersector 	 ,bytepersector  )
	(secpercluster 	 ,secpercluster  )
	(rsvdseccounter	 ,rsvdseccounter )
	(numfats       	 ,numfats        )
	(rootentcnt    	 ,rootentcnt     )
	(totsec16      	 ,totsec16       )
	(media         	 ,media          )
	(fatsz16       	 ,fatsz16        )
	(secpertrk     	 ,secpertrk      )
	(numheads      	 ,numheads       )
	(hiddsec       	 ,hiddsec        )
	(totsec32      	 ,totsec32       )
	(fatsz32       	 ,fatsz32)))))

(load-partition 0 0)
