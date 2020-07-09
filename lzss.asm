; LZSS komprimacni/dekomprimacni modul
; (c) 2002 Rene Puschinger
;----------------------------------------------------------------------------------------------

GENERIC_WRITE           equ 40000000h
GENERIC_READ            equ 80000000h
OPEN_EXISTING           equ 00000003h
FILE_ATTRIBUTE_NORMAL   equ 00000080h
OF_READ                 equ 00000000h
CREATE_ALWAYS           equ 00000002h
FILE_ATTRIBUTE_NORMAL   equ 00000080h
GMEM_FIXED              equ 00000000h
GMEM_ZEROINIT           equ 00000040h
;----------------------------------------------------------------------------------------------

EOF         equ -1                      ; konec souboru
EMPTY       equ -1                      ; prazdny zaznam
NOTFOUND    equ -1                      ; triplet nenalezen
INDEXBITS   equ 12                      ; pocet bitu potrebnych k zakodovani pozice
RINGSIZE    equ 1 shl INDEXBITS
HASHSIZE    equ 5021                    ; velikost hash tabulky, nejlepe prvocislo
BUFSIZE     equ 16384                   ; velikost vstupne/vystupnich bufferu
MINLENGTH   equ 3                       ; minimalni delka retezce, aby mohl byt vyslan na vystup
                                        ; jako par (pozice, delka)
LENGTHBITS  equ 4                       ; pocet bitu potrebnych k zakodovani delky
MAXLENGTH   equ (1 shl LENGTHBITS) + 2
MAXCHAIN    equ 300                     ; nejvetsi delka hash retezce, ktery bude prohledavan
                                        ; (cim vyssi, tim pomalejsi komprimace)

;----------------------------------------------------------------------------------------------

.386
.model flat, stdcall
locals

includelib \TASM\LIB\IMPORT32.LIB

extrn   GlobalAlloc:PROC
extrn   GlobalFree:PROC
extrn   CreateFileA:PROC
extrn   ReadFile:PROC
extrn   WriteFile:PROC
extrn   CloseHandle:PROC

public  C lzcompress
public  C lzdecompress

;----------------------------------------------------------------------------------------------

compute_hash macro
    xor ebx, ebx
    mov ax, [esi]
    mov bx, ax
    mov al, [esi+2]
    sub bl, al
    xor edx, edx
    mov eax, ebx
    mov ebx, HASHSIZE
    div bx
    mov ebx, edx
    shl ebx, 1
endm

;----------------------------------------------------------------------------------------------
    
.data?
align 4
    inpos       dd ?            ; pozice ve vstupnim bufferu
    insize      dd ?            ; skutecna velikost vstupniho bufferu
    outpos      dd ?            ; pozice ve vystupnim bufferu
    out_tst     dd ?            ; 0 pokud posledni zapis probehl v poradku
    id1         dd ?            ; handle pro vstupni a vystupni soubor
    id2         dd ?
    bitbuf      dd ?
    bitbufpos   dd ?
    inbuf       dd ?            ; vstupni
    outbuf      dd ?            ; a vystupni buffer
    hfirst      dd ?            ; hash tabulka
    hlast       dd ?
    lapos       dd ?            ; pozice v lookahead bufferu
    rvalue      dd ?            ; prectena data - ring
    lookahead   dd ?            ; lookahead buffer (je podmnozinou rvalue)
    rprev       dd ?            ; ukazatel na predchozi pozici se stejnym hashem
    rnext       dd ?            ;          na dalsi
    del_flag    db ?            ; indikator - jakmile je nastaven, provadi se mazani
                                ; z ring bufferu i z hash tabulky
    
;----------------------------------------------------------------------------------------------
.code

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lzcompress proc C infp:ptr byte, outfp:ptr byte
; Komprimuje soubor
; Vstup:
;  infp: ukazatel na ASCIIZ retezec s jmenem vstupniho souboru
;  outpf:  ukazatel na ASCIIZ retezec s jmenem vystupniho souboru
; Vraci void

    push ebx
    push ebp
    push esi
    push edi

    call CreateFileA, infp, GENERIC_READ, 0, 0, OPEN_EXISTING, \    ; otevreni souboru pro cteni
        FILE_ATTRIBUTE_NORMAL, 0
    mov [id1], eax
    call CreateFileA, outfp, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, \  ; a pro zapis
        FILE_ATTRIBUTE_NORMAL, 0
    mov [id2], eax

    call GlobalAlloc, GMEM_FIXED, BUFSIZE               ; alokace pameti
    mov [inbuf], eax
    call GlobalAlloc, GMEM_FIXED, BUFSIZE
    mov [outbuf], eax
    call GlobalAlloc, GMEM_FIXED, HASHSIZE*2
    mov [hfirst], eax
    call GlobalAlloc, GMEM_FIXED, HASHSIZE*2
    mov [hlast], eax
    call GlobalAlloc, GMEM_FIXED or GMEM_ZEROINIT, RINGSIZE + MAXLENGTH + 2
    mov [rvalue], eax
    mov [lookahead], eax
    add [lookahead], RINGSIZE
    call GlobalAlloc, GMEM_FIXED, RINGSIZE*2
    mov [rprev], eax
    call GlobalAlloc, GMEM_FIXED, RINGSIZE*2
    mov [rnext], eax

    xor ecx, ecx
    mov edi, [hfirst]               ; inicializace pameti
    mov eax, EMPTY
    mov cx, HASHSIZE
    rep stosw
    mov edi, [hlast]
    mov cx, HASHSIZE
    rep stosw
    mov edi, [rprev]
    mov cx, RINGSIZE/2
    rep stosd
    mov edi, [rnext]
    mov cx, RINGSIZE/2
    rep stosd
    mov [inpos], 0
    mov [insize], 0
    mov [outpos], 0
    mov [del_flag], 0
    mov [bitbuf], 0
    mov [bitbufpos], 0
    mov edi, [lookahead]            ; nacteme prvni triplet
    mov cx, 3
@@1:call read
    stosb
    dec ecx
    jnz @@1
    mov [lapos], 3
    xor edi, edi                    ; di = pozice v ring bufferu
    .while 1                        ; hlavni smycka
        push edi
        mov esi, [lookahead]        ; vypocitame hash index pro novy triplet
        compute_hash                ; ebx = hash index
        or ebp, NOTFOUND
        add ebx, [hlast]
        mov cx, 2
        movzx ebx, word ptr [ebx]
        xor eax, eax                    ; eax = citac kolizi
    @@findloop1:
        cmp bx, EMPTY
        je @@findend        
    @@findloop3:        
        cmp cx, MAXLENGTH
        je @@findend
        mov esi, [lookahead]
        xor edi, edi
        mov di, bx
        add esi, ecx
        add edi, ecx
        mov edx, ecx
        add edi, [rvalue]
        inc edx
        std                         ; porovnavame zprava doleva
    @@4:cmpsb
        jne @@trynext               ; neshoda?
        dec dx
        jnz @@4
        movzx ebp, bx               ; ulozime pozici
        inc ecx                     ; a zvysime delku
        mov edx, eax
        call read
        .if eax == EOF
            mov eax, 1
            cld         
            mov edx, eax
            call putbits
            mov eax, ebp
            mov edx, INDEXBITS
            call putbits
            mov eax, ecx
            sub eax, MINLENGTH
            mov edx, LENGTHBITS
            call putbits
            .break
        .endif
        mov esi, [lookahead]
        inc [lapos]     
        mov byte ptr [esi+ecx], al
        mov eax, edx
        jmp @@findloop3
    @@trynext:
        cmp eax, MAXCHAIN
        je @@findend
        and ebx, 0ffffh
        inc eax
        shl ebx, 1
        add ebx, [rprev]
        mov bx, word ptr [ebx]
        jmp @@findloop1
    @@findend:
        cld
        .if ebp == NOTFOUND
            xor eax, eax
            mov edx, 1
            call putbits
            mov esi, [lookahead]        ; triplet nenalezen, na vystup jde 1. znak z l-a bufferu
            xor eax, eax
            mov al, [esi]
            mov edx, 8
            call putbits
        .else
            mov eax, 1
            mov edx, eax
            call putbits
            mov eax, ebp
            mov edx, INDEXBITS
            call putbits
            mov eax, ecx
            mov edx, LENGTHBITS         
            sub eax, MINLENGTH
            call putbits
            call read
            .if eax == EOF
                .break .if ecx == [lapos]
                xor eax, eax
                mov edx, 1
                call putbits
                xor eax, eax
                mov ebx, [lookahead]
                mov edx, 8
                mov al, byte ptr [ebx+ecx]
                call putbits
                .break
            .endif
            mov ebx, [lookahead]
            add ebx, [lapos]
            mov [ebx], al
            inc [lapos]
        .endif
        pop edi
        xor ebp, ebp                            ; vlozeni retezce po tripletech do ring bufferu
        xor edx, edx
        .while 1
            mov eax, [lapos]
            sub eax, 2
            .break .if ebp == eax               ; uz neni co vlozit?
            .if [del_flag] == 1                 ; provest mazani?
                mov esi, [rvalue]
                add esi, edi                    ; vypocet hash indexu pro stary triplet
                compute_hash
                mov esi, [rnext]
                shl edi, 1                      ; zarovnani kurzoru             
                mov ax, word ptr [esi+edi]      ; ax = dalsi kolize 
                mov esi, [hfirst]
                mov word ptr [esi+ebx], ax
                .if ax == EMPTY
                    mov esi, [hlast]
                    mov word ptr [esi+ebx], ax
                .endif
                mov esi, [rnext]
                xor ebx, ebx
                mov bx, word ptr [esi+edi]
                .if bx != EMPTY
                    mov esi, [rprev]
                    shl ebx, 1
                    mov ax, word ptr [esi+edi]
                    mov word ptr [esi+ebx], ax
                .endif
            .else
                shl edi, 1
            .endif
            mov esi, [lookahead]
            add esi, ebp
            compute_hash
            mov esi, [hlast]
            mov ax, word ptr [esi+ebx]
            mov esi, [rprev]
            mov word ptr [esi+edi], ax
            mov esi, [rnext]
            mov word ptr [esi+edi], EMPTY
            mov esi, [lookahead]
            mov al, [esi+ebp]
            mov esi, [rvalue]
            shr edi, 1                          ; zarovnavame zpet (na byty)
            mov byte ptr [esi+edi], al
            mov esi, [hlast]
            movzx eax, word ptr [esi+ebx]
            .if ax == EMPTY
                mov esi, [hfirst]
                mov word ptr [esi+ebx], di
            .else
                mov esi, [rnext]
                mov word ptr [esi+eax*2], di
            .endif
            mov esi, [hlast]
            mov word ptr [esi+ebx], di
            inc edi
            and edi, RINGSIZE-1
            jnz @@5
            mov [del_flag], 1       ; priste uz budeme mazat
        @@5:inc ebp
        .endw
        push edi
        mov esi, [lookahead]
        mov edi, esi
        add esi, [lapos]
        mov al, byte ptr [esi-2]
        mov [edi], al
        mov al, byte ptr [esi-1]
        mov [edi+1], al
        pop edi
        call read
        mov esi, [lookahead]
        .if eax == EOF
            xor eax, eax
            mov edx, 1
            call putbits            
            xor eax, eax
            lodsb
            mov edx, 8
            call putbits
            xor eax, eax
            mov edx, 1
            call putbits
            xor eax, eax
            lodsb
            mov edx, 8
            call putbits
            push edi                    ; musime znovu ulozit edi na zasobnik !!!
            .break
        .endif
        mov [esi+2], al
        mov [lapos], 3
    .endw

    pop edi

    call finalput                   ; zapis posledniho zbytku bufferu
    
    call CloseHandle, [id1]         ; zavreni obou souboru
    call CloseHandle, [id2]

    call GlobalFree, [inbuf]
    call GlobalFree, [outbuf]
    call GlobalFree, [hfirst]
    call GlobalFree, [hlast]
    call GlobalFree, [rvalue]
    call GlobalFree, [rprev]
    call GlobalFree, [rnext]
    
    pop edi
    pop esi
    pop ebp
    pop ebx

    ret

lzcompress endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

putbits proc
    push ecx
    mov ecx, 32
    push esi
    sub ecx, edx
    lea esi, bitbufpos
    push edi
    sub ecx, [esi]
    shl eax, cl
    or [bitbuf], eax
    add [esi], edx
    lea ecx, outpos
    .while [esi] > 8
        .if [ecx] == BUFSIZE        ; buffer vycerpan?
            call flush
        .endif
        mov edi, [outbuf]
        mov al, byte ptr [bitbuf+3]
        add edi, [ecx]
        inc [ecx]
        mov byte ptr [edi], al
        sub [esi], 8
        shl [bitbuf], 8
    .endw
    pop edi
    pop esi
    pop ecx
    ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

read    proc 
; Precte jeden byte ze souboru.
; Vstup: -
; Vystup: EAX - precteny znak, resp. konec souboru (pokud EAX == EOF)

    push esi
    mov eax, [insize]
    mov esi, [inbuf]
    .if [inpos] == eax      ; je treba nacist novy buffer?
        push ecx
        push edx
        call ReadFile, [id1], esi, BUFSIZE, offset insize, 0
        .if [insize] == 0               ; konec souboru?
            mov eax, EOF
            pop edx
            pop ecx
            jmp @@1
        .endif
        mov [inpos], 0
        pop edx
        pop ecx
    .endif
    add esi, [inpos]
    xor eax, eax
    inc [inpos]
    mov al, [esi]
@@1:pop esi
    ret
read endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

write   proc 
; Zapise jeden byte do souboru
; Vstup: AL - znak pro zapsani
; Vystup: -

    push edi
    mov edi, [outbuf]
    .if [outpos] == BUFSIZE     ; buffer vycerpan?
        call flush
    .endif
    add edi, [outpos]
    mov [edi], al
    inc [outpos]
    pop edi
    ret
write endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

lzdecompress proc C infp:ptr byte, outfp:ptr byte
; Dekomprimuje soubor
; Vstup:
;  infp: ukazatel na ASCIIZ retezec s jmenem vstupniho souboru
;  outpf:  ukazatel na ASCIIZ retezec s jmenem vystupniho souboru
; Vraci void

    push ebx
    push ebp
    push esi
    push edi

    mov [inpos], 0      ; pri opetovnem volani funkce je treba mit tyto hodnoty vzdy nulove
    mov [insize], 0
    mov [outpos], 0
    mov [bitbuf], 0
    mov [bitbufpos], 0

    call GlobalAlloc, GMEM_FIXED, BUFSIZE   ; alokace pameti
    mov [inbuf], eax
    call GlobalAlloc, GMEM_FIXED, BUFSIZE
    mov [outbuf], eax
    call GlobalAlloc, GMEM_FIXED or GMEM_ZEROINIT, RINGSIZE + MAXLENGTH + 2
    mov esi, eax
    mov [lookahead], eax
    mov [rvalue], esi
    add [lookahead], RINGSIZE

    call CreateFileA, infp, GENERIC_READ, 0, 0, OPEN_EXISTING, \    ; otevreni souboru pro cteni
        FILE_ATTRIBUTE_NORMAL, 0
    mov [id1], eax
    call CreateFileA, outfp, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, \  ; a pro zapis
        FILE_ATTRIBUTE_NORMAL, 0
    mov [id2], eax

.while 1
    mov edx, 1
    call getbits
    .break .if eax == EOF           ; konec souboru?
    test eax, eax
    mov ebx, [rvalue]
    jnz @@match                     ; znak nebo retezec?
    mov edx, 8
    call getbits                    ; znak
    .break .if eax == EOF
    mov byte ptr [esi], al
    inc esi                         ; zvyseni pozice v ringu
    call write  
    sub esi, ebx                    ; zarovnani
    and esi, RINGSIZE - 1
    add esi, ebx
    .continue
@@match:
    mov edx, INDEXBITS
    call getbits
    mov ecx, eax
    mov edx, LENGTHBITS
    call getbits
    mov edx, eax
    mov eax, ecx
    mov ecx, edx
    add ecx, MINLENGTH
    push esi
    push ecx
    mov esi, ebx
    add esi, eax
    mov edi, [lookahead]
    push edi
@@2:
    mov al, [esi]
    inc esi
    mov [edi], al
    inc edi
    call write
    dec ecx
    jnz @@2
    pop esi
    pop ecx
    pop edi
@@3:mov al, [esi]
    mov [edi], al
    inc edi
    inc esi
    sub edi, ebx                    ; zarovnani
    and edi, RINGSIZE - 1
    add edi, ebx
    dec ecx
    jnz @@3
    mov ecx, esi
    mov esi, edi
    mov edi, ecx
.endw

    call flush                      ; zapis posledniho zbytku bufferu
    call CloseHandle, [id1]         ; zavreni obou souboru
    call CloseHandle, [id2]

    call GlobalFree, [inbuf]
    call GlobalFree, [outbuf]
    call GlobalFree, [rvalue]

    pop edi
    pop esi
    pop ebp
    pop ebx

    ret
lzdecompress endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

getbits proc
    push ebx
    push ecx
    mov ebx, [bitbufpos]
    push esi    
    .while ebx < edx
        mov esi, [inbuf]
        mov eax, [insize]
        add ebx, 8
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
        mov ecx, 32
        xor eax, eax
        sub ecx, ebx
        mov al, [esi]       
        shl eax, cl
        inc [inpos]
        or [bitbuf], eax
    .endw
    mov ecx, 32
    mov eax, [bitbuf]
    sub ecx, edx
    mov [bitbufpos], ebx    
    shr eax, cl
    mov ecx, edx
    shl [bitbuf], cl
    sub [bitbufpos], ecx
@@1:pop esi
    pop ecx
    pop ebx
    ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

flush   proc 
; Zapise data z vystupniho bufferu do souboru
; Vstup: -
; Vystup: -

    push eax
    push ecx
    push edx
    push esi
    mov esi, [outpos]
    call WriteFile, [id2], [outbuf], esi, offset out_tst, 0
    sub [out_tst], esi      ; pri korektnim zapisu musi out_tst == 0
    mov [outpos], 0
    pop esi
    pop edx
    pop ecx
    pop eax
    ret
flush endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

finalput    proc
    push eax
    push edi
@@2:cmp [bitbufpos], 0
    jle @@1
    .if [outpos] == BUFSIZE
        call flush
    .endif
    mov edi, [outbuf]
    mov al, byte ptr [bitbuf+3]
    add edi, [outpos]
    inc [outpos]
    mov byte ptr [edi], al
    shl [bitbuf], 8
    sub [bitbufpos], 8
    jmp @@2
@@1:call flush
    pop edi
    pop eax
    ret
endp

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;----------------------------------------------------------------------------------------------
end
