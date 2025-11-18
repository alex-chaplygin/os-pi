(defun tiff-header ()
  (&&& 
   #'(lambda (stream)
       (let* ((res (get-word stream))
	      (st (cdr res)))
	 (cons (car res) (make-astream (astream-arr st) (astream-byte-num st) (astream-bit-num st)
				       (if (= 0x4949 (car res)) nil t)))))
   (parse-elem-word 42)
   #'(lambda (stream)
       (let* ((res (get-dword stream))
	      (st (cdr res)))
	 (cons (car res) (stream-seek st (car res)))))))

(defun tiff-directory ()
  (&&&
   #'(lambda (stream)
       (let* ((st (get-word stream))
	      (num (car st))
	      (res (make-array num)))
	 (setq st (cdr st))
	 (for i 0 num
	      (let ((b (get-struct st '((tag . word) (type . word) (count . dword) (offset . dword)))))
		(seta res i (car b))
		(setq st (cdr b))))
	 (cons res stream)))))

(defun tiff ()
  (&&&
   (tiff-header)
   (tiff-directory)))
