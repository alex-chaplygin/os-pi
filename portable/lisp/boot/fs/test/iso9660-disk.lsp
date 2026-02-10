(clear-screen)

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
	   21 0 0 0 ;; root-block-num
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
		       22 0 0 0 ;; block-num
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
		       23 0 0 0 ;; block-num
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
		      24 0 0 0 ;; block-num
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
    (ata-write-sectors 0 0 64 test-sectors)
    (setq test-sectors (make-array 2048))
    (seta test-sectors 0 0)
    (for i 1 6 (seta test-sectors i 55))
    (seta test-sectors 6 1)
    (for i 7 39 (seta test-sectors i 66))
    (for i 39 71 (seta test-sectors i 77))
    (for i 71 2048 (seta test-sectors i 0))
    (ata-write-sectors 0 64 4 test-sectors)
    (ata-write-sectors 0 68 4 primary-descriptor)
    (seta test-sectors 0 255)
    (for i 1 6 (seta test-sectors i 55))
    (seta test-sectors 6 1)
    (for i 7 2048 (seta test-sectors i 0))
    (ata-write-sectors 0 72 4 test-sectors)
    (for i 0 2048 (seta path-table-sectors i 9))
    (ata-write-sectors 0 76 4 path-table-sectors)
    (ata-write-sectors 0 80 4 path-table-sectors)
    (setq test-sectors (make-array 1954))
    (for i 0 1954 (seta test-sectors i 0))
    (ata-write-sectors 0 84 4 (array-cat (array-cat root-entry1 root-entry2) test-sectors))
    (setq test-sectors (make-array 2048))
    (for i 0 2048 (seta test-sectors i 0))
    (arr-set-str test-sectors 0 "content of first file" 21)
    (ata-write-sectors 0 88 4 test-sectors)
    (setq test-sectors (make-array 1998))
    (for i 0 1998 (seta test-sectors i 0))
    (ata-write-sectors 0 92 4 (array-cat dir-entry1 test-sectors))
    (setq test-sectors (make-array 4096))
    (arr-set-str test-sectors 0 "first part of second file on first block" 40)
    (for i 40 2048 (seta test-sectors i 32))
    (arr-set-str test-sectors 2048 "second part of second file on second block" 42)
    (for i 2090 4096 (seta test-sectors i 0))
    (ata-write-sectors 0 96 8 test-sectors)
    ))
(print 'disk-make-start)

(make-iso9660-disk)

(print 'disk-make-end)

(setq cdfsfs (make-instance 'CDFSFileSystem))

(print 'FS-INIT-START)

(fs-init cdfsfs 0 0 300)

(print 'FS-INIT-END)

(print 'LOAD-ROOT-DIR-START)

(load-dir cdfsfs (CDFSFileSystem-root-entry cdfsfs))

(print 'LOAD-ROOT-DIR-END)

(setq file1 (open-file cdfsfs (car (get-hash (CDFSFileSystem-root-entry cdfsfs) 'dir))))

(print (arr-get-str (read-file file1 21) 0 21))

(setq dir (cadr (get-hash (CDFSFileSystem-root-entry cdfsfs) 'dir)))

(print 'LOAD-DIR-START)

(load-dir cdfsfs dir)

(print 'LOAD-DIR-END)

(setq file2 (open-file cdfsfs (car (get-hash dir 'dir))))

(print (arr-get-str (read-file file2 40) 0 40))

(seek-file file2 2048 'SET)

(print (arr-get-str (read-file file2 42) 0 42))

(screen-reset)
