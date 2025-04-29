(defvar *s-box*
  ; s-box для подстановки байтов в AES
  #(99 124 119 123 242 107 111 197 48 1 103 43 254 215 171 118
    202 130 201 125 250 89 71 240 173 212 162 175 156 164 114 192
    183 253 147 38 54 63 247 204 52 165 229 241 113 216 49 21
    4 199 35 195 24 150 5 154 7 18 128 226 235 39 178 117
    9 131 44 26 27 110 90 160 82 59 214 179 41 227 47 132
    83 209 0 237 32 252 177 91 106 203 190 57 74 76 88 207
    208 239 170 251 67 77 51 133 69 249 2 127 80 60 159 168
    81 163 64 143 146 157 56 245 188 182 218 33 16 255 243 210
    205 12 19 236 95 151 68 23 196 167 126 61 100 93 25 115
    96 129 79 220 34 42 144 136 70 238 184 20 222 94 11 219
    224 50 58 10 73 6 36 92 194 211 172 98 145 149 228 121
    231 200 55 109 141 213 78 169 108 86 244 234 101 122 174 8
    186 120 37 46 28 166 180 198 232 221 116 31 75 189 139 138
    112 62 181 102 72 3 246 14 97 53 87 185 134 193 29 158
    225 248 152 17 105 217 142 148 155 30 135 233 206 85 40 223
    140 161 137 13 191 230 66 104 65 153 45 15 176 84 187 22))

(defvar *rcon*
  ; Константы rcon для расширения ключа AES
  #(1 2 4 8 16 32 64 128 27 54 108 216 171 77))

(defun key-info (key)
  ; Определяет параметры AES по длине ключа
  (let ((len (array-size key)))
    (cond ((= len 16) (list 4 10))  ; AES-128
          ;((= len 24) (list 6 12))  ; AES-192
          ;((= len 32) (list 8 14))  ; AES-256
          (t (error "Недопустимая длина ключа")))))

(defun xor-bytes (a b)
  ; XOR двух массивов
  (let* ((len (array-size a))
         (out (make-array len)))
    (for i 0 len
         (seta out i (% (^ (aref a i) (aref b i)) 256)))
    out))

(defun rot-word (arr)
  ; Циклический сдвиг 4-байтового слова
  (let ((out (make-array 4)))
    (seta out 0 (aref arr 1))
    (seta out 1 (aref arr 2))
    (seta out 2 (aref arr 3))
    (seta out 3 (aref arr 0))
    out))

(defun sub-word (arr)
  ; Применение s-box к 4-байтовому слову
  (let ((out (make-array 4)))
    (for i 0 4 (seta out i (aref *s-box* (% (aref arr i) 256))))
    out))

(defun array-copy (a1 a2 ofs count)
  ; Копирует count байт из a1 в a2, начиная с a2[ofs]
  (for i 0 count
       (seta a2 (+ ofs i) (aref a1 i))))

(defun expand-key (key)
  ; Расширение ключа AES на основе длины ключа
  (let* ((params (key-info key))
         (nk (car params))
         (nr (cadr params))
         (nb 4)
         (words (* nb (+ nr 1)))
         (w (make-array (* words 4)))
         (round-keys (make-array (+ nr 1))))

    ;; Копируем начальный ключ
    (array-copy key w 0 (* nk 4))

    ;; Генерация оставшихся слов
    (for i nk words
         (let ((nt (array-seq w (* 4 (- i 1)) (* 4 i))))
           (when (= (% i nk) 0)
             (setf nt (rot-word nt))
             (setf nt (sub-word nt))
             (seta nt 0 ( % (^ (aref nt 0) (aref *rcon* (- (/ i nk) 1))) 256)))
           (when (and (> nk 6) (= (% i nk) 4))
             (setf nt (sub-word nt)))
           (setf nt (xor-bytes nt (array-seq w (* 4 (- i nk)) (* 4 (- i nk -1)))))
           (array-copy nt w (* 4 i) 4)))

    ;; Разбиение по раундам
    (for i 0 (+ nr 1)
         (let ((block (make-array 16)))
           (array-copy (array-seq w (* i 16) (* (+ i 1) 16)) block 0 16)
           (seta round-keys i block)))

    round-keys))

(defun add-round-key (state round-key)
  ; XOR блока с ключом раунда
  (xor-bytes state round-key))

(defun sub-bytes (state)
  ; Применяет S-box к каждому байту состояния
  (let ((out (make-array 16)))
    (for i 0 16
         (seta out i (aref *s-box* (% (aref state i) 256))))
    out))

(defun shift-rows (state)
  ; Сдвигает строки состояния в соответствии со спецификацией AES
  (let ((out (make-array 16)))
    ;;строка 0 нет сдвига
    (for i 0 4
	 (seta out i (aref state i)))
    ;строка 1 — сдвиг влево на 1
    (for i 4 7
	 (seta out i (aref state (++ i))))
    (seta out 7 (aref state 4))
    ;строка 2 — сдвиг влево на 2
    (seta out 8  (aref state 10))
    (seta out 9  (aref state 11))
    (seta out 10 (aref state 8))
    (seta out 11 (aref state 9))
    ;строка 3 — сдвиг влево на 3
    (seta out 12 (aref state 15))
    (for i 13 16
	 (seta out i (aref state (-- i))))
    out))

(defun xtime (b)
  ; Умножение байта на x в GF(2^8)
  (let ((x (% (* b 2) 256)))
    (if (> b 127)
        (^ x 27)
        x)))

(defun mix-columns (state)
  ; Микширование колонок состояния с использованием матричного умножения в GF(2^8)
  (let ((out (make-array 16)))
    (for c 0 4  ; 4 колонки
      (let* ((i (* c 4))
             (s0 (aref state i))
             (s1 (aref state (+ i 1)))
             (s2 (aref state (+ i 2)))
             (s3 (aref state (+ i 3))))
        ;; Формулы MixColumns
        (seta out i       (^ (mul 2 s0) (mul 3 s1) s2 s3))
        (seta out (+ i 1) (^ s0 (mul 2 s1) (mul 3 s2) s3))
        (seta out (+ i 2) (^ s0 s1 (mul 2 s2) (mul 3 s3)))
        (seta out (+ i 3) (^ (mul 3 s0) s1 s2 (mul 2 s3)))))
    out))

(defun encrypt-block (input key)
  ; Шифрует 16-байтовый блок данных с использованием AES
  ;; (unless (= (array-size input) 16)
  ;;   (error "encrypt-block: Длина входного блока должна быть ровно 16 байт"))
  ;; (unless (= (array-size key) 16)
  ;;   (error "encrypt-block: Длина ключа должна быть ровно 16 байт"))

  (let* ((round-keys (expand-key key))
         (state (add-round-key input (aref round-keys 0))))
    (for r 1 11
      (setf state (sub-bytes state))
      (setf state (shift-rows state))
      (when (< r 10)
        (setf state (mix-columns state)))
      (setf state (add-round-key state (aref round-keys r))))
    state))


(defvar *inv-s-box*
  ; Обратная S-box
  #(82 9 106 213 48 54 165 56 191 64 163 158 129 243 215 251
    124 227 57 130 155 47 255 135 52 142 67 68 196 222 233 203
    84 123 148 50 166 194 35 61 238 76 149 11 66 250 195 78
    8 46 161 102 40 217 36 178 118 91 162 73 109 139 209 37
    114 248 246 100 134 104 152 22 212 164 92 204 93 101 182 146
    108 112 72 80 253 237 185 218 94 21 70 87 167 141 157 132
    144 216 171 0 140 188 211 10 247 228 88 5 184 179 69 6
    208 44 30 143 202 63 15 2 193 175 189 3 1 19 138 107
    58 145 17 65 79 103 220 234 151 242 207 206 240 180 230 115
    150 172 116 34 231 173 53 133 226 249 55 232 28 117 223 110
    71 241 26 113 29 41 197 137 111 183 98 14 170 24 190 27
    252 86 62 75 198 210 121 32 154 219 192 254 120 205 90 244
    31 221 168 51 136 7 199 49 177 18 16 89 39 128 236 95
    96 81 127 169 25 181 74 13 45 229 122 159 147 201 156 239
    160 224 59 77 174 42 245 176 200 235 187 60 131 83 153 97
    23 43 4 126 186 119 214 38 225 105 20 99 85 33 12 125))

(defun inv-sub-bytes (state)
  ; Обратное применение S-box
  (let ((out (make-array 16)))
    (for i 0 16
      (seta out i (aref *inv-s-box* (% (aref state i) 256))))
    out))

(defun inv-shift-rows (state)
  ; Обратный сдвиг строк состояния
  (let ((out (make-array 16)))
    ;строка 0 нет сдвига
    (for i 0 4
	 (seta out i (aref state i)))
    ;строка 1 — сдвиг вправо на 1
    (seta out 4 (aref state 7))
    (for i 5 8
	 (seta out i (aref state (-- i))))
    ;строка 2 — сдвиг на 2
    (seta out 8  (aref state 10))
    (seta out 9  (aref state 11))
    (seta out 10 (aref state 8))
    (seta out 11 (aref state 9))
    ;строка 3 — сдвиг влево на 1
    (for i 12 15
	 (seta out i (aref state (++ i))))
    (seta out 15 (aref state 12))
    out))

(defun mul (a b)
  ; Умножение байтов в GF(2^8)
  (let ((p 0))
    (for i 0 8
      (when (!= (% b 2) 0)
        (setf p (^ p a)))
      (setf b (/ b 2))   
      (let ((hi-bit-set (> a 127)))
        (setf a (% (* a 2) 256))
        (when hi-bit-set
          (setf a (^ a 27)))))
    p))

(defun inv-mix-columns (state)
  ; Обратное микширование колонок состояния
  (let ((out (make-array 16)))
    (for c 0 4  ; 4 колонки
      (let* ((i (* c 4))
             (s0 (aref state i))
             (s1 (aref state (+ i 1)))
             (s2 (aref state (+ i 2)))
             (s3 (aref state (+ i 3))))
        ;; Формулы InvMixColumns
        (seta out i       (^ (mul 0x0E s0) (mul 0x0B s1) (mul 0x0D s2) (mul 0x09 s3)))
        (seta out (+ i 1) (^ (mul 0x09 s0) (mul 0x0E s1) (mul 0x0B s2) (mul 0x0D s3)))
        (seta out (+ i 2) (^ (mul 0x0D s0) (mul 0x09 s1) (mul 0x0E s2) (mul 0x0B s3)))
        (seta out (+ i 3) (^ (mul 0x0B s0) (mul 0x0D s1) (mul 0x09 s2) (mul 0x0E s3)))))
    out))

(defun decrypt-block (input key)
  ; Расшифровывает блок данных с использованием AES
  ;; (unless (= (array-size input) 16)
  ;;   (error "decrypt-block: Длина входного блока должна быть ровно 16 байт"))
  ;; (unless (= (array-size key) 16)
  ;;   (error "decrypt-block: Длина ключа должна быть ровно 16 байт"))
  (let* ((round-keys (expand-key key))
         (nr 10)
         (state (add-round-key input (aref round-keys nr))))
    (let ((r 9))
      (while (>= r 0)
        (setf state (inv-shift-rows state))
        (setf state (inv-sub-bytes state))
        (setf state (add-round-key state (aref round-keys r)))
        (when (> r 0)
          (setf state (inv-mix-columns state)))
        (setf r (- r 1))))
    state))

;; Паддинг CMS и шифрование произвольных данных
(defun cms-pad (data)
  "Добавляет CMS-паддинг к массиву байт data. Всегда добавляет хотя бы один блок паддинга."
  (let* ((len     (array-size data))
         (rem     (% len 16))
         ;; вычисляем длину паддинга от 1 до 16
         (pad-len (if (= rem 0) 16 (- 16 rem)))
         (padded  (make-array (+ len pad-len))))
    ;; копируем оригинальные байты
    (for i 0 len
      (seta padded i (aref data i)))
    ;; добавляем pad-len байтов со значением pad-len
    (for i 0 pad-len
	 (seta padded (+ len i) pad-len))
    padded))

(defun cms-unpad (data)
  "Удаляет CMS-паддинг из массива байт data."
  (let* ((len     (array-size data))
         (pad-len (aref data (- len 1)))
         (new-len (- len pad-len)))
    (array-seq data 0 new-len)))

(defun encrypt-data (plaintext key)
  ; Шифрует данные с использованием AES с CMS-паддингом
  (let* ((padded    (cms-pad plaintext))
         (total-len (array-size padded))
         (blocks    (/ total-len 16))
         (out       (make-array total-len)))
    (for i 0 blocks
      (let* ((offset (* i 16))
             (blck   (array-seq padded offset (+ offset 16)))
             (cipher (encrypt-block blck key)))
        (for j 0 16
          (seta out (+ offset j) (aref cipher j)))))
    out))

(defun decrypt-data (ciphertext key)
  ; Расшифровывает данные с использованием AES и удаляет CMS-паддинг
  (let* ((total-len (array-size ciphertext))
         (blocks    (/ total-len 16))
         (out-full  (make-array total-len)))
    (for i 0 blocks
      (let* ((offset (* i 16)) 
             (blck   (array-seq ciphertext offset (+ offset 16)))
             (plain  (decrypt-block blck key)))
        (for j 0 16
          (seta out-full (+ offset j) (aref plain j)))))
    (cms-unpad out-full)))





