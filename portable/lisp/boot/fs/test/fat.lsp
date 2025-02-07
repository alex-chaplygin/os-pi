;модульный тест для fat
(defvar *fat-sector* #(0 0 0 0 ; 0
		       0 0 0 0 ; 1
		       0 0 0 0 ; 2
		       4 0 0 0 ; 3
		       5 0 0 0 ; 4
		       6 0 0 0 ; 5
		       7 0 0 0 ; 6
		       0xf8 0xff 0xff 0xff ; 7
		       0 0 0 0 ; 8
		       0 0 0 0)) ; 9
		    

(defvar *fat-start-sector* 0)
(defvar *disk* 0)
(defvar *free-blocks-count* 5)
(defvar *last-free-block* 8)
(defvar *fat-sectors* 10)

;;заглушка для записи сектора
(defun ata-write-sectors (dev start num arr)
  *fat-sector*)

;;заглушка для чтения сектора, если сектор равен *fs-info-sec* создаем массив из 512 байт, иначе возращаем *fat-sector*
(defun ata-read-sectors (disk sec num)
  (setq *fs-info-sec* nil)
  (if (= sec *fs-info-sec*)
      (make-array 512)
      *fat-sector*))

;;заглушка для обновления последнего свободного блока
(defun update-last-free-block (num)
  (+ *last-free-block* 1))

;; загружаем цепочку из блоков *fat*, возвращает цепочку
(defun get-fat-chain-full-test ()
  (setq *fat-start-sector* 5)
  (setq *fat* '((2 . (3 4))
		(5 . (6 7))
		(8 . ())))
  (print (assert (get-fat-chain 5) '(5 . (6 7)))))

;; загруженная цепочка отсутствует, обновляет объект *fat*
(defun get-fat-chain-test ()
  (setq *fat-start-sector* 5)
  (setq *fat* '((2 . (3 4))
		(8 . ())))
  (print (assert (get-fat-chain 5) '(5 . (6 7)))))

;; добавляем новый блок в цепочку
(defun fat-append-chain-test ()
  (setq *fat* '((3 . (4 5 6 7))))
  (fat-append-chain 3 2)
  (print (assert (get-fat-chain 3) '(3 . (4 5 6 7 2)))))

;;поиск первого свободного блока
(defun fat-get-free-block-test ()
  (print (assert (fat-get-free-block) 8)))



(get-fat-chain-test)
(get-fat-chain-full-test)
(fat-append-chain-test)
(fat-get-free-block-test)
