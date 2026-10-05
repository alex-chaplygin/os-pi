(let ((e (ether-init (pci-config-pack-address 0 3 0))))
  (print (ethernet-pci e))
  (print (ethernet-offset e))
  (print (ethernet-mac e)))
