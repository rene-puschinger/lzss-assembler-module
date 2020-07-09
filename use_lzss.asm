.386
.model flat, stdcall
locals

includelib \TASM\LIB\IMPORT32.LIB

extrn c lzcompress:proc
extrn c lzdecompress:proc
extrn ExitProcess:proc

.data
	in		db 'input.txt', 0
	out		db 'output.lzs', 0
	
.code

start:

	call lzcompress, offset out, offset in

	call ExitProcess, 0

end start
