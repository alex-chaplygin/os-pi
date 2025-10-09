(defun test-pci ()
  (print "PCI read config")
  (for i 0 8
       (print "PCI device: " i (pci-config-read32 0 i 0 8))))

(test-pci)
