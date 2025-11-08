 (defun test-pci ()
  (print "PCI read config")
  (for i 0 8
      (let ((pci (pci-config-pack-address 0 i 0)))
	(print "PCI device: " i
	       (pci-config-read32 pci 0)
	       (get-pci-class pci)
	       (get-pci-bar pci 0)
	       (pci-set-mem-enable pci t)))))

(test-pci)
