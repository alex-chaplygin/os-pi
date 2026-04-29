(defconst +KEY-STATUS+ 0x64) ; номер порта статуса клавиатуры
(defconst +KEY-BUFFER+ 0x60) ; номер порта буфера клавиатуры

(defconst +key-irq+ 1) ; номер линии прерывания клавиатуры
(defconst +key-1+ 0x02)
(defconst +key-2+ 0x03)
(defconst +key-3+ 0x04)
(defconst +key-4+ 0x05)
(defconst +key-5+ 0x06)
(defconst +key-6+ 0x07)
(defconst +key-7+ 0x08)
(defconst +key-8+ 0x09)
(defconst +key-9+ 0x0A)
(defconst +key-0+ 0x0B)
(defconst +key-tab+ 0xf)
(defconst +key-enter+ 0x1c)
(defconst +key-left+ 0x4b)
(defconst +key-right+ 0x4d)
(defconst +key-up+ 0x48)
(defconst +key-down+ 0x50)
;; карта скан код в символ
(defconst +key-map+ #(
 ()  () #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8	;; 9 */
  #\9 #\0 #\- #\= ()	;; Backspace */
  ()			;; Tab */
  #\q #\w #\e #\r	;; 19 */
  #\t #\y #\u #\i #\o #\p #\[ #\] #\
  ;; Enter key */
    ()			;; 29   - Control */
  #\a #\s #\d #\f #\g #\h #\j #\k #\l #\;	;; 39 */
 #\' #\`   ()		;; Left shift */
 #\\ #\z #\x #\c #\v #\b #\n			;; 49 */
  #\m #\, #\. #\/   ()				;; Right shift */
  #\*
    ()	;; Alt */
   #\ 	;; Space bar */
    ()	;; Caps lock */
    ()	;; 59 - F1 key ... > */
    ()   ()   ()   ()   ()   ()   ()   ()
    ()	;; < ... F1() */
    ()	;; 69 - Num lock*/
    ()	;; Scroll Lock */
    ()	;; Home key */
    ()	;; Up Arrow */
    ()	;; Page Up */
  #\-
    ()	;; Left Arrow */
    ()
    ()	;; Right Arrow */
  #\+
    ()	;; 79 - End key*/
    ()	;; Down Arrow */
    ()	;; Page Down */
    ()	;; Insert Key */
    ()	;; Delete Key */
    ()   ()   ()
    ()	;; F11 Key */
    ()	;; F12 Key */
    ()	;; All other keys are undefined */
		      ))
(defvar *keys* (make-array 128)) ; массив нажатий клавиш
(defvar *key-down-handler*) ; обработчик нажатия кнопки
(defvar *key-up-handler*) ; обработчик отпускания кнопки

(defun key-handler ()
  "Простой обработчик прерывания клавиатуры"
  (let ((status (inb +KEY-STATUS+))) ; получает статус есть ли данные в буфере клавиатуры  
    (when (equal (& status 1) 1) ; если есть (младший бит регистра статуса)
      (let ((scan (inb +KEY-BUFFER+))) ; читаем скан код из буфера
	(if (< scan 128)
	    (progn
	      (seta *keys* scan t) ; если меньше 128, то это нажатие клавиши
	      (when *key-down-handler* (funcall *key-down-handler* scan)))
	    (progn
	      (seta *keys* (- scan 128) nil) ; иначе это отпускание клавиши
	      (when *key-up-handler* (funcall *key-up-handler* (- scan 128))))
	    )))
    (interrupt-return))) ;; обязательный примитив возврата из прерывания

(defun key-pressed (key)
  "Вовращает состояние нажатия клавиши key"
  (aref *keys* key))

(defun scan-to-char (scan)
  "Возвращает символ по скан коду"
  (aref +key-map+ scan))

(set-int-handler +key-irq+ #'key-handler)
