(load "../boot/init.lsp")
(load "../boot/reader.lsp")
(load "../boot/parser.lsp")

(defun read-all-tokens (stream)
  (do ((token (read stream nil :eof) (read stream nil :eof))
       (tokens '() (cons token tokens)))
      ((eq token :eof) (reverse tokens))))

(defun get-argv ()
  (cdr sb-ext:*posix-argv*))

(defun main ()
  (let* ((args (get-argv))
         (input-filename (car args))
         (output-filename (cadr args))
         (error-filename (caddr args)))
    (with-open-file (input-stream input-filename :direction :input)
      (let ((tokens (read-all-tokens input-stream)))
        (let* ((res (funcall (parse-some (p-number)) tokens)))
          (if res
              (with-open-file (output-stream output-filename :direction :output :if-exists :supersede)
                (let ((out (car (car res))))
                  (format output-stream "~a" out)))
              (with-open-file (error-stream error-filename :direction :output :if-exists :supersede)
                (format error-stream "Parse error: invalid syntax"))))))))

(main)