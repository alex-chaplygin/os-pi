(defvar f)

(defun fopen-test()
  (setq f (fopen "BOOT/ATA.LSP"))
  (fclose f))

(defun upd-dir-en-test()
  "Обновление даты создания в файле"
  (fopen-test)
  (setf (slot f 'cdate) 20000)
  (update-dir-entry f)
  (let ((b (block-read 3)))
    `(,(aref b 112) ,(aref b 113))))

(defun sec-wr-test(num)
  "Запись и чтение сектора"
  (let ((s (make-array 512)))
    (for i 0 512 (seta s i (& i 0xff)))
    (ata-write-sectors *disk* num 1 s)
    (ata-read-sectors *disk* num 1)))

(defun dn-test ()
  (fat-dname "test.txt"))

(defun fs-info-test ()
  "Тест изменения параметров FSInfo"
  (update-last-free-block (++ *last-free-block*))
  (update-free-blocks-count (-- *free-blocks-count*))
  `(,*last-free-block* ,*free-blocks-count*))

(defun fwrite-test ()
  (let ((a (make-array 256)))
    (for i 0 256 (seta a i i))
    (fwrite f a)))

(defun fwr-test (pos)
  "Тест записи внутри файла num байт"
  (setq f (fopen "BOOT/ATA.LSP"))
  (fseek f pos 'begin)
  (fwrite-test)
  (fseek f pos 'begin)
  (fread f 256))

(defun fwr-r () (fwr-test 10)) ;тест записи обычный
(defun fwr-bl () (fwr-test 1023)) ;тест записи с пересечением границы
(defun fwr-nbl () (fwr-test 5030)) ;тест записи с увеличением размера файла

(defun gfde-test ()
  "тест get-free-dir-entry"
  (get-free-dir-entry '(2)))

(defun cf-test ()
  "Тест создания файла"
  (setq f (create-file "BOOT/1.TXT"))
  (fwrite f #(0x31 0x32 0x33 0x39))
  (fclose f)
  f)

(defun cfr-test ()
  "Тест чтения созданного файла"
  (setq f (fopen "BOOT/1.TXT"))
  (read-text f 4))

(defun cd-test ()
  "Тест создания каталога и файлов в нем"
  (create-dir "TEST")
  (create-file "TEST/1.TXT"))

(defun ws-test ()
  (setq a (make-array 32))
  (setq f (make-fat32file "TEST" 0 0 nil nil (fat-dname "TEST") 0 34 2 160 +directory+ 34 0 0 0 0 0))
  (write-struct a 0 directory-entry f)
  a)

(defun fat-fc-test ()
  "Тест удаления цепочки FAT"
  (get-fat-chain 11))

(print *root-directory*)
(print (listdir "/"))
