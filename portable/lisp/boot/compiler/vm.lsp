(defvar *acc* nil)


(defun lda (val)
  (setq *acc* val))

(defun vm-run (program)
  (if (null program)
      *acc*
      (let ((inst (car program)))
        (unless (null inst)
          (let ((opcode (car inst))
                (ops (cdr inst)))
            (funcall opcode (car ops))))
        (vm-run (cdr program)))))
