;модульный тест для fat
(defvar *fat-sector* #(0 0 0 0 ; 0
		       0 0 0 0 ; 1
		       0 0 0 0 ; 2
		       0 0 0 0 ; 3
		       4 0 0 0 ; 4
		       6 0 0 0 ; 5
		       7 0 0 0 ; 6
		       0xf8 0xff 0xff 0xff ; 7
		       0 0 0 0 ; 8
		       0 0 0 0 ; 9
		       0 0 0 0)) ; 10
(defvar *fat-start-sector* 0)
(defvar *disk* 0)

(defun ata-read-sectors (disk sec num)
  *fat-sector*)


(defun get-fat-chain-full-test ()
  (setq *fat-start-sector* 5)
  (setq *fat* '((2 . (3 4))
		(5 . (6 7))
		(8 . ())))
  (print (assert (get-fat-chain 5) '(5 . (6 7)))))

(defun get-fat-chain-test ()
  (setq *fat-start-sector* 5)
  (setq *fat* '((2 . (3 4))
		(8 . ())))
  (print (assert (get-fat-chain 5) '(5 . (6 7)))))

(get-fat-chain-full-test)
(get-fat-chain-test)
