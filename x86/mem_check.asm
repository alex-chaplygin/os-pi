global physical_memory_size

map_entries equ 0x8000 		;колличество записей
physical_memory_size dd 0
	
mem_check:
	mov di, 0x8004
	xor ebx, ebx
	xor bp, bp
	mov eax, 0xe820		
	mov edx, 0x0534D4150
	mov [es:di + 20], dword 1
	mov ecx, 24
	int 0x15		
	jc short .failed
	mov edx, 0x0534D4150	
	cmp eax, edx
	jne short .failed	
	test ebx, ebx
	je short .failed
	jmp short .jmpin
.e820lp:
	mov eax, 0xe820
	mov [es:di + 20], dword 1
	mov ecx, 24
	int 0x15
	jc short .e820f
	mov edx, 0x0534D4150
.jmpin:
	jcxz .skipent
	cmp cl, 20
	jbe short .notext
	test byte [es:di + 20], 1
	je short .skipent
.notext:
	mov ecx, [es:di + 8]
	or ecx, [es:di + 12]
	jz .skipent
	inc bp
	add [physical_memory_size], ecx
	add di, 24
.skipent:
	test ebx, ebx
	jne short .e820lp
.e820f:
	mov [map_entries], bp
	clc
	ret
.failed:
	stc
	ret
