; функции для работы со строками
(defun search (c str)
  "Найти в строке str позицию первого вхождения символа c"
  (let ((pos nil)
	(size (string-size str)))
    (for i 0 size
	 (when (= (char str (- size i 1)) c)
	   (setq pos (- size i 1))))
    pos))

(defun search-back (c str)
  "Найти в строке str позицию последнего вхождения символа c"
  (let ((pos nil)
	(size (string-size str)))
    (for i 0 size
	 (when (= (char str i) c)
	   (setq pos i)))
    pos))

(defun split (del str)
  "Разделить строку str на элементы по символу разделителю del"
  (let ((f (search del str)))
    (if (null f) (list str)
	(cons (subseq str 0 f)
	      (if (= f (- (string-size str) 1)) (list "")
		  (split del (subseq str (+ f 1) (string-size str))))))))

(defun implode (char-list)
  "Преобразует список печатных символов в строку"
  (let* ((len (list-length char-list))
	 (str (make-string len #\ )))
    (for i 0 len
	 (sets str i (nth char-list i)))
    str))

(defun explode (str)
  "Создаёт список из печатных символов строки"
  (let ((out-list NIL)
	(iter (string-size str)))
    (while (> iter 0)
      (setq iter (- iter 1))
	 	 (setq out-list (cons (char str iter) out-list)))
    out-list))

(defun is-alpha (sym)
  "Предикат проверки на букву"
  (let ((c (char-code sym)))
    (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))))

(defun is-digit (sym)
  "Предикат проверки на цифру"
  (let ((c (char-code sym)))
    (and (>= c 48) (<= c 57))))

(defun toupper (char)
  "Преобразовать символ char в верхний регистр"
  (let ((c (char-code char)))
    (if (and (>= c (char-code #\a))
             (<= c (char-code #\z)))
        (code-char (- c (- (char-code #\a) (char-code #\A))))
        char)))

(defun strtoint (str base)
  "Конвертирует строку str в число в системе счисления base"
  (let ((res 0))
    (for i 0 (string-size str)
         (let* ((char (toupper (char str i)))
                (c (char-code char))
                (c-num (+ (- c (char-code #\A)) 10)))
           (if (>= c-num 10)
               (unless (< c-num base)
                 (error "strtoint: invalid digit"))
               (setq c-num (- c (char-code #\0))))
           (setq res (+ (* res base) c-num))))
    res))

(defun safe-strtoint (str base)
  "Безопасное преобразование строки в 32-битное знаковое целое.
   Возвращает число или nil, если переполнение или недопустимая цифра."
  (let ((res 0))
    (let ((i 0))
      (let ((len (string-size str)))
        (let ((max-val 2147483647))
          (while (< i len)
            (let ((char (toupper (char str i))))
              (let ((c (char-code char)))
                (let ((digit (if (>= c (char-code #\A))
                                 (+ (- c (char-code #\A)) 10)
                                 (- c (char-code #\0)))))
                  (if (or (< digit 0) (>= digit base))
                      (setq i len) ; break
                      (progn
                        ; Проверка: res > (max_val - digit) / base  → переполнение
                        (if (> res (/ (- max-val digit) base))
                            (setq i len) ; break — переполнение
                            (setq res (+ (* res base) digit))))))))
            (setq i (+ i 1)))
          (if (= i len) res nil))))))

(defun strtofloat (str)
  "Преобразует строку в число с плавающей точкой"
  (let* ((parts (split #\. str))
         (int-part (strtoint (car parts) 10))
         (frac-part-str (second parts)))
    (if (= frac-part-str "")
        (* 1.0 int-part)
	(let ((frac-part (strtoint frac-part-str 10)))
	  (+ int-part (/ frac-part (expt 10.0 (string-size frac-part-str))))))))

(defun is-hex-sym (sym)
  "Предикат проверки на символ шестнадцатеричного числа"
  (let ((c (char-code sym)))
    (or (is-digit sym)
        (and (>= c (char-code #\a)) (<= c (char-code #\f)))
        (and (>= c (char-code #\A)) (<= c (char-code #\F))))))


(defun is-delimiter-p (c delimiters)
  "Проверяет, является ли символ c разделителем из списка delimiters."
  (labels ((check (lst)
             (if (null lst)
                 nil ; Базовый случай: дошли до конца списка, элемента нет
                 (if (= c (car lst))
                     T ; Базовый случай: нашелся элемент
                     (check (cdr lst)))))) ; Рекурсивный шаг
    (check delimiters)))

(defun numbertostr (n)
  "Преобразует число в строку"
  (if (= n 0) "0"
    (let ((res nil))
      (while (> n 0)
        (setq res (cons (code-char (+ (% n 10) 48)) res))
        (setq n (/ n 10)))
      (implode res))))

(defun string-upcase (str)
  "Преобразует строку в верхний регистр"
  (let ((new-str (make-string (string-size str) #\ )))
    (for i 0 (string-size str)
      (sets new-str i (toupper (char str i))))
    new-str))

(defun is-default-delimiter-p (c)
  "Проверяет, является ли символ c разделителем по умолчанию (пробел или новая строка)."
  (is-delimiter-p c (list #\  (code-char 10))))

(defun is-whitespace (c)
  (or (eq c #\ ) (eq c (code-char 10)) (eq c (code-char 9))))
