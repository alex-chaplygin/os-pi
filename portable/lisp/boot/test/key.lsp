; имитация контроллера клавиатуры
(defconst +size+ 256)
(defvar *keysdata* (make-array +size+))
(defvar *key-pos* 0)

(for i 0 +size+
     (seta *keysdata* i i))

(defun inb (port)
  (case port
    (+KEY-STATUS+ (if (= *key-pos* +size+) 0 1))
    (+KEY-BUFFER+ (let ((s (aref *keysdata* *key-pos*)))
		   (incf *key-pos*)
		   s))))
