(defun parse-suc (val)
  "Элементарный парсер - успешный разбор со значением val"
  #'(lambda (stream) (cons val stream)))

(defun parse-fail ()
  "Элементарный парсер - неудачный разбор. Возвращает nil"
  #'(lambda (stream) nil))

(defun parse-pred (pred)
  "Парсер по предикату. Возвращает символ или nil."
  #'(lambda (stream)
      (if (null stream) nil
	(let ((res (get-byte stream)))
	  (if (null res) nil
	    (if (funcall pred (car res)) res nil))))))


(defun parse-elem (sym)
  "Элементарный парсер, ожидающий заданный элемент из потока"
  (parse-pred #'(lambda (x) (= x sym))))

(defun &&& (&rest parsers)
  "Последовательный комбинатор. Возвращает nil, если любой из парсеров вернул nil."
  #'(lambda (stream)
      (labels ((apply-parser (parsers stream res)
		 (if (null parsers) (cons res stream)
		     (let ((parser-res (funcall (car parsers) stream)))
		       (if (null parser-res) nil
			   (apply-parser (cdr parsers) (cdr parser-res)
                                         (append res (list (car parser-res)))))))))
	(apply-parser parsers stream nil))))

(defun parse-or (&rest parsers)
  "Параллельный комбинатор. Пробует парсеры по очереди. Если ни один не сработал,
   выбрасывает фатальную ошибку."
  (unless parsers (error "parse-or: no parsers"))
  #'(lambda (stream)
      (labels ((apply-parser (parsers stream)
		 (if (null parsers) 
		     (throw 'parse-error "parse-or: All alternatives failed")
		   (let ((parser-res (funcall (car parsers) stream)))
		     (if (null parser-res) (apply-parser (cdr parsers) stream) parser-res)))))
      (apply-parser parsers stream))))
		     
(defun parse-app (parser f)
  "Комбинатор применения функции к результату разбора. Возвращает nil при неудаче."
  #'(lambda (stream)
      (let ((r (funcall parser stream)))
	(if (null r) nil (cons (funcall f (car r)) (cdr r))))))

(defun parse-return (res)
  #'(lambda (x) res))

(defun parse-many (parser)
  "Комбинатор - 0 или более повторений. Никогда не возвращает nil."
  #'(lambda (stream)
      (labels ((apply (stream res)
		      (let ((parser-res (funcall parser stream)))
			(if (null parser-res) (cons res stream)
			  (apply (cdr parser-res) (append res (list (car parser-res))))))))
	      (apply stream nil))))

(defun parse-optional (parser)
  "Комбинатор: 0 или 1 применение парсера. Всегда успешен."
  #'(lambda (stream)
      (let ((res (funcall parser stream)))
        (if res res
            (cons nil stream)))))

(defun parse-some (parser)
  "Комбинатор - 1 или более повторений. Возвращает nil при неудаче."
  (parse-app (&&& parser (parse-many parser))
	     #'(lambda (x) (cons (car x) (nth x 1)))))

(defmacro parse-rec (parser)
  "Комбинатор для рекурсивных парсеров"
  `#'(lambda (stream) (funcall ,parser stream)))

(defun skip-spaces ()
  "Пропуск 0 или более пробелов"
  (parse-many (parse-elem #\ )))


(defun parse-decimal (&rest delimiters)
  "Разбор десятичного числа. Если список разделителей не задан, используются пробел и \n."
  (let ((dels (if (null delimiters)
                  (list #\  (code-char 10)) ; Разделители по умолчанию
                  (car delimiters))))
    #'(lambda (stream)
        (let ((r (funcall
                   (&&& (parse-optional (parse-elem #\-))
                        (parse-some (parse-pred #'is-digit)))
                   stream)))
          (if (null r)
              (throw 'parse-error "parse-decimal: Not a valid number structure")
              (let* ((parts (car r)) (rest-stream (cdr r)) (peek (get-byte rest-stream)))
                (if (or (null peek) (is-delimiter-p (car peek) dels))
                    (let ((minus (car parts)) (digits (nth parts 1)))
                      (let ((num-str (implode digits)))
                        (cond
                          ((and minus (= num-str "2147483648")) (cons -2147483648 rest-stream))
                          (t (let ((num (safe-strtoint num-str 10)))
                               (if (null num)
                                   (throw 'parse-error "parse-decimal: Invalid integer format or overflow")
                                   (cons (if minus (- num) num) rest-stream)))))))
                    (throw 'parse-error "parse-decimal: Number not followed by a delimiter or end-of-stream"))))))))

(defun parse-hex (&rest delimiters)
  "Разбор шестнадцатеричного числа. Если список разделителей не задан, используются пробел и \n."
  (let ((dels (if (null delimiters)
                  (list #\  (code-char 10))
                  (car delimiters))))
    #'(lambda (stream)
        (let ((r (funcall
                   (&&& (parse-elem #\0) (parse-elem #\x)
                        (parse-some (parse-pred #'is-hex-sym)))
                   stream)))
          (if (null r)
              (throw 'parse-error "parse-hex: Not a valid hex structure (missing '0x' or digits)")
              (let* ((parts (car r)) (rest-stream (cdr r)) (peek (get-byte rest-stream)))
                (if (or (null peek) (is-delimiter-p (car peek) dels))
                    (let ((digits (nth parts 2)))
                      (let ((num-str (implode digits)))
                        (let ((num (safe-strtoint num-str 16)))
                          (if (null num)
                              (throw 'parse-error "parse-hex: Invalid hexadecimal format or overflow")
                              (cons num rest-stream)))))
                    (throw 'parse-error "parse-hex: Number not followed by a delimiter or end-of-stream"))))))))


;(defun parse-separated (parser separator-parser)
; "Парсит одно или более вхождение parser, разделенное separator-parser"
;  (parse-app (&&& parser (parse-many (&&& separator-parser parser)))
;	     #'(lambda (x)
;                 (labels ((map-cadr (lst)
;                            (if (null lst)
;                                nil
;                                (cons (cadr (car lst)) (map-cadr (cdr lst))))))
;                   (cons (car x) (map-cadr (cadr x)))))))