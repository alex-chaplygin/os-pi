OUTPUT_FORMAT(elf32-i386)
ENTRY(start)
SECTIONS
 {
   . = 0x100000;
   .boot : {
   	  *(.multiboot_header)
	}
   .text : { *(.text) }
   .data : { *(.data) }
   .lisp : { _lisp_start = .;
   	     *(.lisp*)
	   }
   .bss  : { *(.bss)  }
 }
