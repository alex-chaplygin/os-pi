;; Модуль работы с протоколом Ethernet (номера регистров в константы)
;; Сетевая карта Realtek RTL8169
;; Доки RTL8139: https://www.futurlec.com/Datasheet/Realtek/RTL8139.pdf

(defconst +realtek-pci-vendor+ 0x10EC) ; производитель устройства Realtek
(defconst +rtl8139-pci-device+ 0x8139) ; устройство RTL8169
(defconst +rtl8139-reset-addres+ 0x37) ; смещение 0037h (см. док. стр. 14, п. 6.4)
(defconst +rtl8139-reset-byte+ 0x10) ; байт записи сброса, необходим 4-ый бит равный 1 (см. док. стр. 14, п. 6.4)

(defclass ethernet () (pci offset mac)) ; класс ethernet

(defun ether-init (pci)
    "Функция инциализирует сетевую карту RTL8139 и возвращает объект ethernet."
    (let ((id (get-pci-vendor-device pci)))
      (unless (and (= (car id) +realtek-pci-vendor+) (= (cdr id) +rtl8139-pci-device+))
	(error "RTL8139 not found"))
      (pci-set-mem-enable pci t) ; даем доступ к памяти
      (let ((offset (get-pci-bar pci 0))) ; получаем BAR0 - место, где записаны регистры карты
	(outb (+ offset +rtl8139-reset-addres+) +rtl8139-reset-byte+) ; пишем в байт CR 0x10 т.к. это бит отвечате за сброс карты
	(while (!= (& (inb (+ offset +rtl8139-reset-addres+)) +rtl8139-reset-byte+) 0) nil) ; ждем, пока бит не станет 0, что будет значить завершение сброса
	(let ((mac (make-array 6))) ; собираем MAC адрес он со смещениями 0000h - 0005h (см. док. стр. 10, п. 6)
	  (for i 0 6
	       (seta mac i (- (inb (+ offset i)) offset)))
	  (make-ethernet pci offset mac))))) ; собираем и возвращаем объект
