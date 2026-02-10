(deftest block-set-size-test () ()
         "Провера установки значение размера блока"
         (set-block-size 1)
         (assert (and (= *block-size* 512) (= *block-sectors* 1)) t))

;; ;; Проверка на ошибку при установке значение размера блока
;; (set-block-size "not a number")

(deftest block-set-offset () ()
         "Проверка установки значение смещение блоков"
         (set-block-offset 1)
         (assert *block-sector-offset* 1))

;; ;; Проверка на ошибку при установке значение смещение блоков
;; (set-block-offset 15.5)

;; ;; Проверка на ошибку при чтении блока не в зоне блоков
;; (set-blocks-start-num 6)
;; (block-read 6)

(deftest block-read-test () ()
         "Проверка чтения блока"
         (let ((test-sector (make-array 512)))
           (for i 0 512
                (seta test-sector i (& 1 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 6)
           (ata-write-sectors 0 7 1 test-sector)
           (assert (block-read 6) test-sector)))

(deftest block-read-test2 () ()
         "Проверка чтения блока из нескольких секторов"
         (let ((test-sector (make-array 512)))
           (for i 0 512
                (seta test-sector i (& 1 0xff)))
           (set-block-size 2)
           (set-block-offset 2)
           (set-blocks-start-num 6)
           (ata-write-sectors 0 14 1 test-sector)
           (ata-write-sectors 0 15 1 test-sector)
           (assert (block-read 6) (array-cat test-sector test-sector))))

(deftest block-write-test () ()
         "Проверка записи блока"
         (let ((test-sector (make-array 512)))
           (for i 0 512
                (seta test-sector i (& 2 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 6)
           (block-write 7 test-sector)
           (assert (ata-read-sectors 0 8 1) test-sector)))

(deftest block-write-test2 () ()
         "Проверка записи блока из нескольких секторов"
         (let ((test-block (make-array 1024)))
           (for i 0 1024
                (seta test-block i (& 2 0xff)))
           (set-block-size 2)
           (set-block-offset 2)
           (set-blocks-start-num 6)
           (block-write 7 test-block)
           (assert (array-cat (ata-read-sectors 0 16 1) (ata-read-sectors 0 17 1)) test-block)))

(deftest get-blocks-pos-test () ()
         "Проверка получения индекса блок и смещения в блоке"
         (let ((blocks '(10 9 8 7 6 5 4 3 2 1))
               (offset 2060))
           (set-block-size 1)
           (assert (get-blocks-pos blocks offset) '(4 . 12))))

(deftest get-offset-from-pos-test () ()
         "Проверка получения смещения в байтах из позиции"
         (assert (get-offset-from-pos '(4 . 12)) 2060))
