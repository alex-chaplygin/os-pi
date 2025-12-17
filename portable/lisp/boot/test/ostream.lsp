(unit-tests 'ostream)

(defun get-data (s)
  "Возвращает записанную часть массива"
  (let* ((len (OStream-ptr s)); Узнаем длину
         (res (make-array len)))  ; Создаем массив этой длины
    (for i 0 len                      
       (seta res i (aref (OStream-data s) i)))
    res)) 

(deftest byte-test ()
  "Тест байтов"
  (let ((s (new-stream)))
    (write-byte s 10)
    (write-byte s 20)
    
    ;; Сравниваем с массивом #(10 20)
    (print (assert (get-data s) #(10 20)))))

(deftest word-test ()
  "Тест слова (Little Endian)"
  (let ((s (new-stream)))
    (write-word s 0xAABB)
    
    (print (assert (get-data s) #(0xBB 0xAA)))))

(deftest dword-test ()
  "Тест двойного слова (Little Endian)"
  (let ((s (new-stream)))
    (write-dword s 0x11223344)
    
    (print (assert (get-data s) #(0x44 0x33 0x22 0x11)))))

(deftest fifo-test ()
  "Тест FIFO (4 числа по 4 байта)"
  (let ((s (new-stream)))
    ;; Записываем заголовок как в драйвере
    (write-dword s 16)   ; MIN
    (write-dword s 1024) ; MAX
    (write-dword s 16)   ; NEXT
    (write-dword s 16)   ; STOP

    ;; 16   -> 16 0 0 0
    ;; 1024 -> 0  4 0 0
    ;; 16   -> 16 0 0 0
    ;; 16   -> 16 0 0 0
    (print (assert (get-data s) 
                   #(16 0 0 0   0 4 0 0   16 0 0 0   16 0 0 0)))))

(run-tests)