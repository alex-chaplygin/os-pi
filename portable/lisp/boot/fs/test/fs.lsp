(deftest close-file-test () ()
         "Проверка закрытия файла"
         (let ((file (make-file "test" 512 '(0 . 0) '(6) nil)))
           (close-file file)
           ;;(assert file '(()))
           (assert file #(0 0 0 0 0 0))
              ))

(deftest tell-file-test () ()
         "Проверка получения позиции в файле"
         (let ((file (make-file "test" 512 '(4 . 12) '(6) nil)))
           (assert (tell-file file) 2060)))

(deftest seek-file-test1 () ()
         "Проверка установки позиции в файле от начала"
         (let ((file (make-file "test" 1024 '(0 . 0) '(6 7) nil)))
           (seek-file file 524 'SET)
           (assert (File-position file) '(1 . 12))))

(deftest seek-file-test2 () ()
         "Проверка установки позиции в файле от текущей позиции с переходом между блоками"
         (let ((file (make-file "test" 1024 '(0 . 12) '(6 7) nil)))
           (seek-file file 512 'CUR)
           (assert (File-position file) '(1 . 12))))

(deftest seek-file-test2-2 () ()
         "Проверка установки позиции в файле от текущей позиции без переходов между блоками"
         (let ((file (make-file "test" 512 '(0 . 12) '(6) nil)))
           (seek-file file 400 'CUR)
           (assert (File-position file) '(0 . 412))))

(deftest seek-file-test3 () ()
         "Проверка установки позиции в файле от конца файла"
         (let ((file (make-file "test" 1024 '(0 . 0) '(6 7) nil)))
           (seek-file file 0 'END)
           (assert (File-position file) '(2 . 0))))

;; ;; проверка на ошибку при выходе за пределы
;; (let ((file (make-file "test" 512 '(0 . 0) '(6) nil)))
;;   (seek-file file 1 'END))

(deftest is-directory-test1 () ()
         "Проверка на каталог у файла"
         (let ((file (make-file "test" 512 '(0 . 0) '(6) nil)))
           (assert (is-directory file) nil)))

(deftest is-directory-test2 () ()
         "Проверка на каталог у каталога"
         (let ((file (make-file "test" 512 '(0 . 0) '(6) t)))
           (assert (is-directory file) t)))

(deftest read-file-test1 () ()
         "Проверка на чтении файла с начала файла"
         (let ((test-sector (make-array 512))
               (file (make-file "test" 512 '(0 . 0) '(10) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 10 test-sector)
           (setq file-buf (read-file file 512))
           (assert (cons file-buf (File-position file))
		   (cons test-sector '(1 . 0)))))

(deftest read-file-test2 () ()
         "Проверка на чтении файла не с начала файла"
         (let ((test-sector (make-array 512))
               (file (make-file "test" 512 '(0 . 120) '(11) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 11 test-sector)
           (setq file-buf (read-file file 390))
           (assert (cons file-buf (File-position file))
		   (cons (array-seq test-sector 120 510)
			 '(0 . 510)))))

(deftest read-file-test3 () ()
         "Проверка на чтении файла не с начала файла с переходом между блоками"
         (let ((test-sector (make-array 512))
               (file (make-file "test" 1024 '(0 . 1) '(12 13) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 12 test-sector)
           (block-write 13 test-sector)
           (setq file-buf (read-file file 1022))
           (assert (cons file-buf (File-position file))
		   (cons (array-cat (array-seq test-sector 1 512)
                                    (array-seq test-sector 0 511))
			 '(1 . 511)))))

;; ;; Проверка на выход за пределы чтения файла
;; (let ((test-sector (make-array 512))
;;       (file (make-file "test" 512 '(0 . 1) '(11) nil))
;;       (file-buf nil))
;;   (for i 0 512
;;        (seta test-sector i (& i 0xff)))
;;   (set-block-size 1)
;;   (set-block-offset 1)
;;   (set-blocks-start-num 10)
;;   (block-write 11 test-sector)
;;   (setq file-buf (read-file file 512)))

(deftest write-file-test1 () ()
         "Проверка на запись в файл с начала файла"
         (let ((test-sector (make-array 512))
               (file (make-file "test" 512 '(0 . 0) '(14) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector i (& i 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (write-file file test-sector)
           (assert (cons (block-read 14) (File-position file))
		   (cons test-sector '(1 . 0)))))

(deftest write-file-test2 () ()
         "Проверка на запись в файл не с начала файла"
         (let ((test-sector1 (make-array 512))
               (test-sector2 (make-array 512))
               (file (make-file "test" 512 '(0 . 256) '(14) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector1 i (& i 0xff))
                (seta test-sector2 i (& 99 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 14 test-sector1)
           (write-file file (array-seq test-sector2 256 512))
           (assert (cons (block-read 14) (File-position file))
		   (cons
                    (array-cat (array-seq test-sector1 0 256)
                               (array-seq test-sector2 256 512))
                    '(1 . 0)))))

(deftest write-file-test3 () ()
         "Проверка на запись в файл не с начала файла c переходом между блоками"
         (let ((test-sector1 (make-array 512))
               (test-sector2 (make-array 512))
               (file (make-file "test" 1024 '(0 . 511) '(14 15) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector1 i (& i 0xff))
                (seta test-sector2 i (& 99 0xff)))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 14 test-sector1)
           (block-write 15 test-sector1)
           (write-file file test-sector2)
           (assert (cons (array-cat (block-read 14) (block-read 15)) (File-position file))
		   (cons
                    (array-cat (array-cat (array-seq test-sector1 0 511)
					  test-sector2)
                               (array-seq test-sector1 511 512))
                    '(1 . 511)))))

(deftest write-file-test4 () ()
         "Проверка на запись в файл не с начала файла до конца файла c переходом между блоками"
         (let ((test-sector1 (make-array 512))
               (test-sector2 (make-array 513))
               (file (make-file "test" 1024 '(0 . 511) '(14 15) nil))
               (file-buf nil))
           (for i 0 512
                (seta test-sector1 i (& i 0xff))
                (seta test-sector2 i (& 99 0xff)))
           (seta test-sector2 512 (& 99 0xff))
           (set-block-size 1)
           (set-block-offset 1)
           (set-blocks-start-num 10)
           (block-write 14 test-sector1)
           (block-write 15 test-sector1)
           (write-file file test-sector2)
           (assert (cons (array-cat (block-read 14) (block-read 15)) (File-position file))
		   (cons
                    (array-cat (array-seq test-sector1 0 511)
                               test-sector2)
                    '(2 . 0)))))

;; ;; Проверка на ошибку записи в файл
;; (let ((test-sector (make-array 512))
;;       (file (make-file "test" 512 '(0 . 256) '(14) nil))
;;       (file-buf nil))
;;   (for i 0 512
;;        (seta test-sector i (& i 0xff)))
;;   (set-block-size 1)
;;   (set-block-offset 1)
;;   (set-blocks-start-num 10)
;;   (write-file file test-sector))
