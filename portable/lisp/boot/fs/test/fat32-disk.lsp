(defvar *bios-parameter-block* #(144 144 144
                                 32 32 32 32 32 32 32 32
                                 0 2
                                 4
                                 3 0
                                 2
                                 0 0
                                 255 255
                                 0
                                 1 0
                                 0 0
                                 0 0
                                 0 0 0 0
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
  (seta fat-table-small 3 5)
  (seta fat-table-small 5 16)
  (seta fat-table-small 16 150)
  (seta fat-table-small 150 151)
  (seta fat-table-small 151 +fat-block-end+)

  (seta fat-table-small 200 152)
  (seta fat-table-small 152 127)
  (seta fat-table-small 127 160)
  (seta fat-table-small 160 9)
  (seta fat-table-small 9 100)
  (seta fat-table-small 100 +fat-block-end+)
  (for i 0 256
       (arr-set-num
        *fat-table-sectors*
        (* i 4)
        (aref fat-table-small i)
        4)))

(setq *disk* 0)
(handle
 (progn
   
   (print "DISK INIT START")
   (ata-write-sectors *disk* 0 1 *bios-parameter-block*)
   (ata-write-sectors *disk* 1 1 *fsinfo-struct*)
   (ata-write-sectors *disk* 2 1 *bios-parameter-block*)
   (ata-write-sectors *disk* 3 2 *fat-table-sectors*)
   (ata-write-sectors *disk* 5 2 *fat-table-sectors*)
   (print "DISK INIT END")

   (print "FS-INIT START")
   (setq *file-system* (make-instance 'FAT32FileSystem))
   (fs-init *file-system* *disk* 0 65535)
   (print "FS-INIT END")

   (print "EMPTY ROOT DIR LOAD")
   (load-dir *file-system* (FAT32FileSystem-root-entry *file-system*))

   (let ((root-dir (FAT32FileSystem-root-entry *file-system*))
	 (test-dir1 nil)
	 (test-dir2 nil)
	 (test-file1 nil)
	 (test-file2 nil)
	 (test-file3 nil))
     (print "CREATE FIRST FILE AND FIRST DIR")
     (setq test-file1 (create-file* *file-system* root-dir "first-file.txt"))
     (setq test-dir1 (create-dir* *file-system* root-dir "first-dir"))
     (print "OPEN FIRST FILE")
     (let ((file (open-file* *file-system* test-file1))
           (test-str "first file content")
           (test-buf (make-array 18)))
       (arr-set-str test-buf 0 test-str 18)
       (print "FIRST FILE WRITE")
       (write-file file test-buf)
       (seek-file file 0 'SET)
       (print "FIRST FILE READ")
       (print (arr-get-str (read-file file 18) 0 18)))
     
     (print "EMPTY FIRST DIR LOAD")
     (load-dir *file-system* test-dir1)
     (print "CREATE SECOND FILE IN FIRST DIR")
     (setq test-file2 (create-file* *file-system* test-dir1 "second file.bin"))
     (print "OPEN SECOND FILE")
     (let ((file (open-file* *file-system* test-file2))
           (test-str "second file content")
           (test-buf (make-array 19)))
       (arr-set-str test-buf 0 test-str 19)
       (print "SECOND FILE WRITE")
       (write-file file test-buf)
       (seek-file file 0 'SET)
       (print "SECOND FILE READ")
       (print (arr-get-str (read-file file 19) 0 19)))
     
     (print "CREATE SECOND DIR")
     (setq test-dir2 (create-dir* *file-system* test-dir1 "second_dir"))
     (print "EMPTY SECOND DIR LOAD")
     (load-dir *file-system* test-dir2)
     (print "CREATE THIRD FILE IN SECOND DIR")
     (setq test-file3 (create-file* *file-system* test-dir2 "third file with veeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeery long name"))

     (print "OPEN THIRD FILE")
     (let ((file (open-file* *file-system* test-file3))
           (test-str "third file content")
           (test-buf (make-array 19)))
       (arr-set-str test-buf 0 test-str 19)
       (print "THIRD FILE WRITE")
       (write-file file test-buf)
       (seek-file file 0 'SET)
       (print "THIRD FILE READ")
       (print (arr-get-str (read-file file 19) 0 19)))))
 (error (message)
	(print "error:" message)))
