;; константы
NULLOBJ equ 4
	
;; внешние символы
extern t, frame_reg, boot_load, boot_code, parse, prims, nprims, new_pair

%include "macro.inc"

global run
%include "/tmp/code"
	UNALIGN
	ret
