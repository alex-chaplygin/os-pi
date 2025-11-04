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

;; Ожидание заданного маркера
(defun marker (marker)
  #'(lambda (stream)
      (let ((word (get-word stream)))
	(if (= (car word) marker) word nil))))

;;    Quantization ::= DQT Lq PTq Q[64] ; таблица квантования
(defun quantization ()
  (parse-app (marker DQT) #'(lambda (x) 'quantization)))

;;    Huffman ::= DHT Lh Tch L[16] V[sum[L]] ; таблица Хаффмана
(defun huffman ()
  (parse-app (marker DHT) #'(lambda (x) 'huffman)))

;;    App ::= APP Lp A[Lp] ; данные приложения
(defun application ()
  (parse-app (marker APP)  #'(lambda (x) 'app)))

;;    Restart ::= DRI Lr Ri ; интервал перезапуска
(defun restart ()
  (parse-app (marker DRI) #'(lambda (x) 'restart)))

;;    Comment ::= COM Lc C[Lc] ; комментарий
(defun comment ()
  (parse-app (marker COM) #'(lambda (x) 'comment)))

;;    Table ::= Quantization | Huffman | Restart | App | Comment ; таблицы
(defun table ()
  (parse-or (quantization) (huffman) (application) (restart) (comment)))

;;    JPEG ::= SOI Frame EOI
(defun jpeg ()
  (&&& (marker SOI) (table)))
