;; константы
NULLOBJ equ 4
MARK_BIT equ 5			; как в objects.h
ARRAY equ 6			; как в objects.h
MAX_ARGS equ 16	
	
;; внешние символы
extern t, frame_reg, boot_load, boot_code, parse, prims, nprims, new_pair, new_empty_array, new_function, new_prim_function, call_form

%include "macro.inc"

global run
%include "/tmp/code"
	UNALIGN
	ret
