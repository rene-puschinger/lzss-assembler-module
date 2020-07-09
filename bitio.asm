BUFSIZE					equ 16384
EOF						equ -1

GENERIC_WRITE			equ 40000000h
GENERIC_READ			equ 80000000h
OPEN_EXISTING			equ 00000003h
FILE_ATTRIBUTE_NORMAL	equ 00000080h
OF_READ					equ 00000000h
CREATE_ALWAYS			equ 00000002h
FILE_ATTRIBUTE_NORMAL	equ 00000080h
GMEM_FIXED				equ 00000000h
GMEM_ZEROINIT			equ 00000040h

includelib \TASM\LIB\IMPORT32.LIB

.386
.model flat, stdcall
locals

extrn	GlobalAlloc:PROC
extrn	GlobalFree:PROC
extrn	CreateFileA:PROC
extrn	ReadFile:PROC
extrn	WriteFile:PROC
extrn	CloseHandle:PROC

public	C in_assign
public	C out_assign
public	C bitio_done
public	C getbits
public	C putbits
public	C finalput
public	C flush

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

.data?
	id1			dd ?
	id2			dd ?
	inpos		dd ?
	insize		dd ?
	outpos		dd ?
	inbuf		dd ?
	outbuf		dd ?
	bitbuf		dd ?
	bitbufpos	dd ?
	out_tst		dd ?
	
.code

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

in_assign	proc C infp:ptr byte
	push eax
	call CreateFileA, infp, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
	mov [id1], eax
	call GlobalAlloc, GMEM_FIXED, BUFSIZE
	mov [inbuf], eax
	mov [inpos], 0
	mov [insize], 0
	mov [bitbuf], 0
	mov [bitbufpos], 0
	mov	[id2], 0
	mov [outbuf], 0
	pop eax
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

out_assign	proc C outfp:ptr byte
	push eax
	call CreateFileA, outfp, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
	mov [id2], eax
	call GlobalAlloc, GMEM_FIXED, BUFSIZE
	mov [outbuf], eax
	mov [outpos], 0
	mov [bitbuf], 0
	mov [bitbufpos], 0
	mov [id1], 0
	mov [inbuf], 0
	pop eax
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

bitio_done	proc C
	.if [id1] != 0
		call CloseHandle, [id1]
	.else 
		call CloseHandle, [id2]
	.endif
	.if [inbuf] != 0
		call GlobalFree, [inbuf]
	.else
		call GlobalFree, [outbuf]
	.endif
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

getbits	proc C size:byte
	push ebx
	push ecx
	push edx
	push esi
	mov ebx, [bitbufpos]
	movzx edx, [size]
	.while ebx < edx
		add ebx, 8
		mov esi, [inbuf]
		mov eax, [insize]
		.if [inpos] == eax
			push ebx
			push edx
			call ReadFile, [id1], esi, BUFSIZE, offset insize, 0
			.if [insize] == 0
				mov eax, EOF
				pop edx
				pop ebx
				jmp @@1
			.endif
			mov [inpos], 0
			pop edx
			pop ebx
		.endif
		add esi, [inpos]
		xor eax, eax
		lodsb
		inc [inpos]
		mov ecx, 32
		sub ecx, ebx
		shl eax, cl
		or [bitbuf], eax
	.endw
	mov [bitbufpos], ebx
	mov ecx, 32
	sub ecx, edx
	mov eax, [bitbuf]
	shr eax, cl
	mov ecx, edx
	shl [bitbuf], cl
	sub [bitbufpos], ecx
@@1:pop esi
	pop edx
	pop ecx
	pop ebx
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

putbits	proc C size:byte, value:word
	push ecx
	push edx
	push esi
	push edi
	mov	ecx, 32
	movzx edx, [size]
	push edx
	sub ecx, edx
	lea esi, bitbufpos
	sub ecx, [esi]
	movzx edx, [value]
	shl edx, cl
	or [bitbuf], edx
	pop edx
	add [esi], edx
	.while [esi] > 8
		.if [outpos] == BUFSIZE		; buffer vycerpan?
			call flush
		.endif
		mov edi, [outbuf]
		add edi, [outpos]
		mov al, byte ptr [bitbuf+3]
		mov byte ptr [edi], al
		inc [outpos]
		shl [bitbuf], 8
		sub [esi], 8
	.endw
	pop edi
	pop esi
	pop edx
	pop ecx
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

finalput	proc C
	push edi
@@2:cmp [bitbufpos], 0
	jle @@1
	.if [outpos] == BUFSIZE
		call flush
	.endif
	mov edi, [outbuf]
	add edi, [outpos]
	mov al, byte ptr [bitbuf+3]
	mov byte ptr [edi], al
	inc [outpos]
	shl [bitbuf], 8
	sub [bitbufpos], 8
	jmp @@2
@@1:call flush
	pop edi
	ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

flush	proc C
	push eax
	push ecx
	push edx
	push esi
	mov esi, [outpos]
	call WriteFile, [id2], [outbuf], esi, offset out_tst, 0
	sub [out_tst], esi		; pri korektnim zapisu musi out_tst == 0
	mov [outpos], 0
	pop esi
	pop edx
	pop ecx
	pop eax
	ret
flush endp
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end
