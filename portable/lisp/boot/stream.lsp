;; Класс потока на основе строки
(defclass SStream ()
  (str ; строка
   index ; индекс текущего символа
   ))

(defun stream-from-str (str)
  "Создает поток из строки str"
  (make-SStream str 0))

(defmethod get-byte ((self SStream))
  "Чтение очередного символа из потока
   Возвращает точчную пару (символ . новое состояние потока) или nil если конец потока"
  (let ((str (SStream-str self))
	(index (SStream-index self)))
    (if (= index (string-size str)) nil
      (cons (char str index) (make-SStream str (++ index))))))
      
  

(let* ((stream (stream-from-str "a"))
       (s (get-byte stream))
       (sym (car s))
       (s2 (cdr s)))
  (print stream)
  (print sym s2)
  (print (get-byte s2)))
