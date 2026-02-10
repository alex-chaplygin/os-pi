(deftest ata-test () ()
         "Проверка работы интерфейса ata на чтение и запись сектора"
         (let ((test-sector (make-array 512)))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (ata-write-sectors 0 0 1 test-sector)
           (assert (ata-read-sectors 0 0 1) test-sector)))
