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
