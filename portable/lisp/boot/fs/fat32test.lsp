(defvar f)

(defun fopen-test()
  (setq f (fopen "BOOT/ATA.LSP")))

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
