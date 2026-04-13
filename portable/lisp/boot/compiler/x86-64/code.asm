;; константы
NULLOBJ equ 4
	
;; внешние символы
extern t
extern frame_reg

%include "macro.inc"

section .text
global run
%include "/tmp/code"
	UNALIGN
	ret
