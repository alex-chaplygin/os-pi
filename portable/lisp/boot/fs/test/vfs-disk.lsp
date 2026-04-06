(defvar *mbr-struct* #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ;; код загрузчика
                       128 ;; раздел занят
                       0 0 0
                       12 ;; тип файловой системы FAT32
                       0 0 0
                       1 0 0 0 ;; первый сектор
                       250 0 0 0 ;; количество секторов
                       128 ;; раздел занят
                       0 0 0
                       148 ;; тип файловой системы ISO9660(CDFS)
                       0 0 0
                       252 0 0 0 ;; первый сектор
                       250 0 0 0 ;; количество секторов
                       0 ;; раздел свободен
                       0 0 0
                       0
                       0 0 0
                       0 0 0 0
                       0 0 0 0
                       0
                       0 0 0
                       0
                       0 0 0
                       0 0 0 0
                       0 0 0 0
                       0xAA 0x55))

(defvar *bios-parameter-block* #(144 144 144
                                 32 32 32 32 32 32 32 32
                                 0 2
                                 4
                                 3 0
                                 2
                                 0 0
                                 250 0
                                 0
                                 1 0
                                 0 0
                                 0 0
                                 1 0 0 0
                                 0 0 0 0
                                 2 0 0 0
                                 0 0 ;; fat flags
                                 0 0
                                 2 0 0 0
                                 1 0
                                 2 0
                                 0 0 0 0 0 0 0 0 0 0 0 0
                                 0
                                 0
                                 40
                                 0 0 0 0
                                 0 0 0 0 0 0 0 0 0 0 0
                                 32 32 32 32 32 32 32 32
                                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                 85 170))

(defvar *fsinfo-struct* #(82 82 97 65
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                          114 114 65 97
                          255 255 255 255
                          2 0 0 0
                          0 0 0 0 0 0 0 0 0 0 0 0
                          0 0 85 170))

(defvar *fat-table-sectors* (make-array 1024))
(let ((fat-table-small (make-array 256)))
  (for i 0 256 (seta fat-table-small i +fat-block-free+))
  (seta fat-table-small 2 +fat-block-end+)
  (for i 0 256
       (arr-set-num
        *fat-table-sectors*
        (* i 4)
        (aref fat-table-small i)
        4)))

(defun make-iso9660-disk ()
  (let ((primary-descriptor-add (make-array 1165))
	(primary-descriptor1
	 #(1
	   2 2 2 2 2
	   1
	   0
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   0 0 0 0 0 0 0 0
	   0 0 0 1 ;; volume-size
	   1 0 0 0
	   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0
	   0 0 0 0
	   0 8 ;; block-size
	   8 0
	   0 8 0 0 ;; path-table-size
	   0 0 8 0
	   76 0 0 0 ;; path-table-sector
	   0 0 0 0
	   0 0 0 80
	   0 0 0 0
	   34 ;; root-dir-entry start
	   0
	   84 0 0 0 ;; root-block-num
	   0 0 0 21
	   94 0 0 0 ;; root-size
	   0 0 0 94
	   45 5 9 0 43 0 0 ;; root-creation-date-time
	   2 ;; root-attributes
	   0
	   0
	   0 1
	   1 0
	   1 ;; root-name-len
	   0 ;; root-name
           ;;root-dir-entry end
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
	   49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
	   49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
	   49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
	   49 57 57 49 49 50 51 49 50 48 53 57 53 57 0 0 0
	   1
	   0))
	(test-sectors nil)
        (path-table-sectors (make-array 2048))
        (root-entry1 #(
		       ;; dir-entry start
		       50 ;; dir-entry-len
		       0
		       85 0 0 0 ;; block-num
		       0 0 0 22
		       21 0 0 0 ;; size
		       0 0 0 21
		       125 1 7 12 0 0 0 ;; creation-date-time
		       0 ;; attributes
		       0
		       0
		       0 1
		       1 0
		       16 ;; name-len
		       102 105 114 115 116 45 102 105 108 101 46 116 120 116 59 49 ;; name first-file.txt;1
		       0 ;; padding
		       ;;dir-entry end
		       ))
        (root-entry2 #(
		       ;; dir-entry start
		       44 ;; dir-entry-len
		       0
		       86 0 0 0 ;; block-num
		       0 0 0 22
		       50 0 0 0 ;; size
		       0 0 0 50
		       125 4 7 22 30 0 0 ;; creation-date-time
		       2 ;; attributes directory
		       0
		       0
		       0 1
		       1 0
		       11 ;; name-len
		       102 105 114 115 116 45 100 105 114 59 49 ;; name first-dir;1
		       ;;dir-entry end
		       ))
        (dir-entry1 #(
		      ;; dir-entry start
		      50 ;; dir-entry-len
		      0
		      87 0 0 0 ;; block-num
		      0 0 0 22
		      42 8 0 0 ;; size
		      0 0 8 42
		      125 5 12 4 0 0 0 ;; creation-date-time
		      0 ;; attributes
		      0
		      0
		      0 1
		      1 0
		      17 ;; name-len
		      115 101 99 111 110 100 45 102 105 108 101 46 116 120 116 59 49 ;; name second-file.txt;1
		      ;;dir-entry end
		      )
          (primary-descriptor nil))
        )
    (for i 0 1165 (seta primary-descriptor-add i 0))
    (setq primary-descriptor (array-cat primary-descriptor1 primary-descriptor-add))
    (setq test-sectors (make-array 32768))
    (for i 0 32768 (seta test-sectors i 0))
    (ata-write-sectors 0 252 64 test-sectors)
    (setq test-sectors (make-array 2048))
    (seta test-sectors 0 0)
    (for i 1 6 (seta test-sectors i 55))
    (seta test-sectors 6 1)
    (for i 7 39 (seta test-sectors i 66))
    (for i 39 71 (seta test-sectors i 77))
    (for i 71 2048 (seta test-sectors i 0))
    (ata-write-sectors 0 316 4 test-sectors)
    (ata-write-sectors 0 320 4 primary-descriptor)
    (seta test-sectors 0 255)
    (for i 1 6 (seta test-sectors i 55))
    (seta test-sectors 6 1)
    (for i 7 2048 (seta test-sectors i 0))
    (ata-write-sectors 0 324 4 test-sectors)
    (for i 0 2048 (seta path-table-sectors i 9))
    (ata-write-sectors 0 328 4 path-table-sectors)
    (ata-write-sectors 0 332 4 path-table-sectors)
    (setq test-sectors (make-array 1954))
    (for i 0 1954 (seta test-sectors i 0))
    (ata-write-sectors 0 336 4 (array-cat (array-cat root-entry1 root-entry2) test-sectors))
    (setq test-sectors (make-array 2048))
    (for i 0 2048 (seta test-sectors i 0))
    (arr-set-str test-sectors 0 "content of first file" 21)
    (ata-write-sectors 0 340 4 test-sectors)
    (setq test-sectors (make-array 1998))
    (for i 0 1998 (seta test-sectors i 0))
    (ata-write-sectors 0 344 4 (array-cat dir-entry1 test-sectors))
    (setq test-sectors (make-array 4096))
    (arr-set-str test-sectors 0 "first part of second file on first block" 40)
    (for i 40 2048 (seta test-sectors i 32))
    (arr-set-str test-sectors 2048 "second part of second file on second block" 42)
    (for i 2090 4096 (seta test-sectors i 0))
    (ata-write-sectors 0 348 8 test-sectors)
    ))

(print "MBR INIT")
(ata-write-sectors *disk* 0 1 *mbr-struct*)

(print "FAT32 PARTITION INIT")
(ata-write-sectors *disk* 1 1 *bios-parameter-block*)
(ata-write-sectors *disk* 2 1 *fsinfo-struct*)
(ata-write-sectors *disk* 3 1 *bios-parameter-block*)
(ata-write-sectors *disk* 4 2 *fat-table-sectors*)
(ata-write-sectors *disk* 6 2 *fat-table-sectors*)

(print "CDFS PARTITION INIT")
(make-iso9660-disk)


(handle
 (progn

   (print "FAT32 load partition")
   (load-partition 0 0)

   (print "Create first file named 'first-file.txt'")
   (create-file "first-file.txt")

   (let ((file (open-file "first-file.txt"))
         (test-buf (make-array 41)))
     (arr-set-str test-buf 0 "wrote then read the content of first file" 41)
     (print "write to file")
     (write-file file test-buf)
     (seek-file file 0 'SET)
     (print "read the content of file:" (arr-get-str (read-file file 41) 0 41)))

   (print "Create dir named 'firstdir'")
   (create-dir "firstdir")

   (print "change-dir to new dir")
   (change-dir "firstdir")

   (print "create second file named 'second file.bin'")
   (create-file "second file.bin")

   (let ((file (open-file "second file.bin"))
         (test-buf (make-array 50)))
     (arr-set-str test-buf 0 "wrote then read the content of second created file" 50)
     (print "write to file")
     (write-file file test-buf)
     (seek-file file 0 'SET)
     (print "read the content of file:" (arr-get-str (read-file file 50) 0 50)))

   (print "CDFS load partition")
   (load-partition 0 1)

   (print "open first file")
   (let ((file (open-file "first-file.txt")))
     (print (arr-get-str (read-file file 21) 0 21)))

   (print "change-dir to second dir")
   (change-dir "first-dir")

   (print "open second file")
   (let ((file (open-file "second-file.txt")))
     (print (arr-get-str (read-file file 40) 0 40))
     (seek-file file 2048 'SET)
     (print (arr-get-str (read-file file 42) 0 42)))
  )
 (error (message)
	(print "error:" message)))

