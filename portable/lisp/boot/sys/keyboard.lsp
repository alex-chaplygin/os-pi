(defconst +key-irq+ 1)

(set-int-handler +key-irq+ '(lambda ()
			     (let ((status (inb 0x64)))
			       (when (= (& status 1) 1)
				 (let ((scan (inb 0x60)))
				   (when (< scan 128)
				     (print scan)))))))
