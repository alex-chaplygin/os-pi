(defconst SOI  0xFFD8) ;Начало изображения
(defconst EOI  0xFFD9)
(defconst SOS  0xFFDA)
(defconst DQT  0xFFDB)
(defconst DNL  0xFFDC)
(defconst DRI  0xFFDD)
(defconst DHP  0xFFDE)
(defconst EXP  0xFFDF)
(defconst APP  0xFFE0)
(defconst JPG  0xFFF0)
(defconst COM  0xFFFE)
(defconst SOF  0xFFC0)
(defconst DHT  0xFFC4)
(defconst RST  0xFFD0)

(defun read-marker (marker)
  "Ожидание маркера marker"
  (if (= (eval marker) (get-word)) t (error `(,marker not found))))

(defun read-quant-table ()
  t)

(defun read-huff-table()
  t)

(defun read-restart-interval()
  t)

(defun read-comment()
  t)

(defun read-app()
  t)

(defun read-tables ()
  "Чтение таблиц"
  (let ((marker (get-word)))
    (when (case marker
	      (DQT (read-quant-table))
	      (DHT (read-huff-table))
	      (DRI (read-restart-interval))
	      (COM (read-comment))
	      (APP (read-app))
	      (otherwise nil))
      (read-tables))))

(defun read-frame ()
  "Чтение кадра"
  (read-tables))
