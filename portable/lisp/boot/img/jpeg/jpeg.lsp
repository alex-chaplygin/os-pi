(defconst SOI  0xFFD8) ;Начало изображения
(defconst EOI  0xFFD9) ;Конец изображения
(defconst SOS  0xFFDA) ;Начало скана
(defconst DQT  0xFFDB) ;Определитель таблицу квантизации
(defconst DNL  0xFFDC) ;Определитель кол-ва строк 
(defconst DRI  0xFFDD) ;Определитель интервала перезапуска
(defconst DHP  0xFFDE) ;Определитель иерархической прогрессии
(defconst APP  0xFFE0) ;Зарезервировано для сегментов приложенй
(defconst JPG  0xFFF0) ;Зарезервировано для расширений JPEG
(defconst COM  0xFFFE) ;Комментарий
(defconst SOF  0xFFC0) ;Начало основного кадра JPEG
(defconst DHT  0xFFC4) ;Определитель таблицы Хаффмана
(defconst RST  0xFFD0) ;Интервал перезапуска параметров

;; Таблицы квантования
(defvar *quantization-tables*) 
;; Таблицы Хаффмана
(defvar *huffman-tables*) 

(defun jpeg-huf-dc (id) (get-hash *huffman-tables* (cons 0 id))) ;; получить таблицу DC с номером id
(defun jpeg-huf-ac (id) (get-hash *huffman-tables* (cons 1 id))) ;; получить таблицу AC с номером id
(defun jpeg-quant (id) (get-hash *quantization-tables* id)) ;; получить квантования с номером id

(defun jpeg-init ()
  "Инициализация структур данных"
  (setq *pred* #(0 0 0))
  (setq *quantization-tables* (make-hash))
  (setq *huffman-tables* (make-hash)))

;; Ожидание заданного маркера
(defun marker (marker) (parse-elem-word marker))

(defun read-length ()
  "Читает длину и пропускает длина минус 2 байта"
  (&&& len-> #'get-word (parse-array (- len 2))))

;;    Quantization ::= DQT Lq PTq Q[64] ; таблица квантования
(defun quantization ()
  (&&& (marker DQT)
       lq-> #'get-word
       PTq-> #'get-4bit
       Q-> (parse-array 64)
       res-> #'(lambda (stream) (cons nil (stream-seek stream (- lq 67) 'seek-cur)))
       return (set-hash *quantization-tables* (cdr PTq) Q)))

;;    Huffman ::= DHT Lh Tch L[16] V[sum[L]] ; таблица Хаффмана
(defun huffman ()
  (&&& (marker DHT) #'get-word
       Tch-> #'get-4bit
       L->(parse-array 16)
       V->(parse-array (array-sum L))
       return (set-hash *huffman-tables* Tch (huff-make-code-lens L V))))

;;    App ::= APP Lp A[Lp] ; данные приложения
(defun application ()
  (parse-app (&&& (marker APP) (read-length)) #'(lambda (x) (list 'app (second x)))))

;;    Restart ::= DRI Lr Ri ; интервал перезапуска
(defun restart ()
  (parse-app (marker DRI) #'(lambda (x) 'restart)))

;;    Comment ::= COM Lc C[Lc] ; комментарий
(defun comment ()
  (parse-app (marker COM) #'(lambda (x) 'comment)))

;;    Table ::= Quantization | Huffman | Restart | App | Comment ; таблицы
(defun table ()
  (parse-or (quantization) (huffman) (application) (restart) (comment)))

;;FrameHeader ::= SOF Lf P Y X Nf FComp[Nf] ; заголовок кадра
;;FComp ::= Cf HV Tq ; компонент кадра
(defun frame-header ()
  (&&& (marker SOF)
       st->(parse-struct '((lf . word) (p . byte) (y . word) (x . word) (nf . byte)))
       ch->(parse-many-n 3 (parse-struct '((c . byte) (hv . bits4) (tq . byte))))
       return (cons st (list-to-array ch))))

;; ScanHeader ::= SOS Ls Ns SComp[Ns] Ss Se Ahl ; заголовок скана
(defun scan-header ()
  (&&& (marker SOS) #'get-word
       ns-> #'get-byte
       params-> (parse-many-n ns (parse-struct '((cs . byte) (tda . bits4))))
       #'get-byte #'get-byte #'get-4bit
       return (list-to-array params)))

;; JPEG ::= SOI Frame EOI
;; Frame ::= Table* FrameHeader Scan[Nf???]; кадр
;; Scan ::= Dnl? Table* ScanHeader ESC* ; скан с раделителем Dnl
;; Scomp ::= Cs Tda ; компонент скана
(defun jpeg ()
  (&&& (marker SOI) (parse-many (table)) frame->(frame-header) (parse-many (table)) scan->(scan-header)
       mcu->(parse-many-n (compute-mcu frame) (decode-mcu (jpeg-hufs scan) (jpeg-quants (cdr frame)) (get-hash (car frame) 'jpeg-hv)))
       (marker EOI)
       return (labels ((make-matrix (w h) (array-map (make-array h) #'(lambda (x) (make-array w)))))
		(let* ((par (car frame))
		       (width2 (get-hash par 'width2))
		       (height2 (get-hash par 'height2))
		       (mcu-h (get-hash par 'mcu-hcount))
		       (mcu-v (get-hash par 'mcu-vcount))
		       (mcu-width (get-hash par 'mcu-width))
		       (mcu-height (get-hash par 'mcu-height))
       		       (y (make-matrix width2 height2))
       		       (cb (make-matrix width2 height2))
       		       (cr (make-matrix width2 height2))
		       (yy 0))
       		  (for mcu-y 0 mcu-v
		       (let ((x 0))
			 (for mcu-x 0 mcu-h
			      (put-mcu (car mcu) y cb cr x yy (get-hash par 'jpeg-hv))
			      (setq mcu (cdr mcu) x (+ x mcu-width))))
		       (setq yy (+ yy mcu-height)))
       		  (ycbcr-to-rgb y cb cr (get-hash par 'x) (get-hash par 'y))))))

(defun decode-jpeg (jpeg)
  "Декодировать массив байт JPEG"
  (jpeg-init)
  (let ((j (funcall (jpeg) (stream-from-arr jpeg t))))
    (if j (car j) nil)))
