IDEAL
MODEL 	small
STACK 	100h
P186
JUMPS

; Variables/Data segment
; -------------------------------------------------------------------------------------------------
DATASEG
		white       equ	15			;white
		black       equ	0			;black
		cyan        equ	11			;cyan
		yellow      equ  14			;yellow

		keyHeight   equ 200
		keyWidth    equ 9
		
		smallHeight equ 100
		smallWidth  equ 4   ; must be EVEN!
		smallY      equ 101

		initX       equ 3

		right       equ 1
		left        equ 2
		mid         equ 3
		blackKey    equ 4

		; -------------------------------------------------------------------------------------------------
		; misc vars
		blackKeyHelper dw blackKey
		; -------------------------------------------------------------------------------------------------

		
		; -------------------------------------------------------------------------------------------------
		; utility vars for storing "current" variables (current color, current key, etc.)
		currColor      dw 0  ; the color identifying the key
		currKey        dw 0
		currFreq       dw 0
		currPos        dw 0
		effectiveColor dw 0  ; the color we want to use (for example, if we are playing a sound we want to color the key in Yellow)
		currChar       dw 0
		; -------------------------------------------------------------------------------------------------

		
		; -------------------------------------------------------------------------------------------------
		; right - 1, left - 2, mid - 3, black - 4
		keysArr  dw 3 dup (right, blackKey, mid, blackKey, left,         right, blackKey, mid, blackKey, mid, blackKey, left)
		keysLocs dw 36 dup (initX) ; location of all keys. ALL INITIALIZED TO THE FIRST X VALUE (REST WILL BE CHANGED IN THE CODE)
		keyCount equ 36
		; -------------------------------------------------------------------------------------------------


		; -------------------------------------------------------------------------------------------------
		;             1st part    ;     '     2nd part          black keys
		keyBoard db 'ASDFGHJKL', 03Bh, 027h, 'ZXCVBNM,.\',  'QWERTYUIOP[]0-=', '$'
		; keyBoard dw 65,83, 68, 70, 71, 72, 74, 75, 76, 03Bh, 027h;, 'zxcvbnm,./',  'qwertyuiop[]0-=', '$'
		; -------------------------------------------------------------------------------------------------

		
		; -------------------------------------------------------------------------------------------------
		; frequencies for 36 key keyboard: https://learn.digilentinc.com/Documents/400
		; assembly list copied from: https://stackoverflow.com/questions/34500138/playing-music-with-the-ibm-pc-speaker
		c5      =   2280        ;  523.2510 hz
		cis5    =   2152        ;  554.3650 hz
		d5      =   2032        ;  587.3300 hz
		dis5    =   1918        ;  622.2540 hz
		e5      =   1810        ;  659.2550 hz
		f5      =   1708        ;  698.4560 hz
		fis5    =   1612        ;  739.9890 hz
		g5      =   1522        ;  783.9910 hz
		gis5    =   1437        ;  830.6090 hz
		a5      =   1356        ;  880.0000 hz
		ais5    =   1280        ;  932.3280 hz
		b5      =   1208        ;  987.7670 hz
		c6      =   1140        ; 1046.5000 hz
		cis6    =   1076        ; 1108.7300 hz
		d6      =   1016        ; 1174.6600 hz
		dis6    =    959        ; 1244.5100 hz
		e6      =    905        ; 1318.5100 hz
		f6      =    854        ; 1396.9100 hz
		fis6    =    806        ; 1479.9800 hz
		g6      =    761        ; 1567.9800 hz
		gis6    =    718        ; 1661.2200 hz
		a6      =    678        ; 1760.0000 hz
		ais6    =    640        ; 1864.6600 hz
		b6      =    604        ; 1975.5300 hz
		c7      =    570        ; 2093.0000 hz
		cis7    =    538        ; 2217.4600 hz
		d7      =    508        ; 2349.3200 hz
		dis7    =    479        ; 2489.0200 hz
		e7      =    452        ; 2637.0200 hz
		f7      =    427        ; 2793.8300 hz
		fis7    =    403        ; 2959.9600 hz
		g7      =    380        ; 3135.9600 hz
		gis7    =    359        ; 3322.4400 hz
		a7      =    339        ; 3520.0000 hz
		ais7    =    320        ; 3729.3100 hz
		b7      =    302        ; 3951.0700 hz

		; ordered frequencies in a list
		freqs dw c5, cis5, d5, dis5, e5, f5, fis5, g5, gis5, a5, ais5, b5, c6, cis6, d6, dis6, e6, f6, fis6, g6, gis6, a6, ais6, b6, c7, cis7, d7, dis7, e7, f7, fis7, g7, gis7, a7, ais7, b7
		; -------------------------------------------------------------------------------------------------


		;================================
		; showing bmp file
		filename db 'piano.bmp',0
		filehandle dw ?
		Header db 54 dup (0)
		Palette db 256*4 dup (0)
		ScrLine db 320 dup (0)
		ErrorMsg db 'Error', 13, 10 ,'$'
		;================================


; Code segment
; -------------------------------------------------------------------------------------------------
CODESEG


; draw horizontal line, left starting at (arg1, arg2) of length arg3. color is arg4
; push color (param4)
; push length (param3)
; push y (param2)
; push x (param1)
; bp+10 => param3 | bp+8 => param3  | bp+6 => param2 | bp+4 => param1
proc horizontal
		push bp
		mov bp, sp
		pusha

		mov cx, [bp+4]
		add cx, [bp+8]

		mov dx, [bp+6]     ; row
		mov al, [bp+10]     ; color

hPixelDrawLoop:
		mov ah, 0ch    ; put pixel
		int 10h

		dec cx
		cmp cx, [bp+4]
		jae hPixelDrawLoop

		popa
		pop	bp
		ret
endp horizontal

; draw vecrtical line, top starting at (arg1, arg2) of length arg3. color is arg4
proc vertical
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+6]
		add dx, [bp+8]

		mov cx, [bp+4]     ; col
		mov al, [bp+10]     ; color

	vPixelDrawLoop:
		mov ah, 0ch    ; put pixel
		int 10h

		dec dx
		cmp dx, [bp+6]
		jae vPixelDrawLoop

		popa
		pop	bp
		ret
endp vertical


; draw a rectangle, top left at (arg1, arg2) of keyWidth arg3 and keyHeight arg4. color is arg5
; use the vertical procedure only
proc rectangle
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+8]     ; dx tells us the keyWidth
		add dx, [bp+4]     ; add the x coord

		push [bp+12]    ; push the color
		push [bp+10]     ; push the keyHeight
		push [bp+6]     ; push the starting y

	hLineDrawLoop:
		push dx
		call vertical
		add sp, 2    ; (pop)   remove the keyWidth (dx) from the stack
		dec dx
		cmp dx, [bp+4]
		jae hLineDrawLoop

		add sp, 6  ; remove top 3 values of stack

		popa
		pop	bp
		ret
endp rectangle
		
; draw a piano key looking like "L". x position is (arg1=bp+4)	
proc drawRightKey
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+4]     ; dx tells us the x coord
		
		push [effectiveColor] ; color
		push keyHeight ; keyHeight
		push keyWidth ; keyWidth
		push 1 ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		add dx, keyWidth  
		
		push [effectiveColor] ; color
		push smallHeight ; keyHeight
		push smallWidth ; keyWidth
		push smallY ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		popa
		pop	bp
		ret
endp drawRightKey


; draw a piano key looking like a *reversed* "L". x position is (arg1=bp+4)	
proc drawLeftKey
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+4]     ; dx tells us the x coord

		push [effectiveColor] ; color
		push smallHeight ; keyHeight
		push smallWidth ; keyWidth
		push smallY ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		add dx, smallWidth     

		push [effectiveColor] ; color
		push keyHeight ; keyHeight
		push keyWidth ; keyWidth
		push 1 ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		popa
		pop	bp
		ret

endp drawLeftKey



; draw a piano key looking like "⊥". x position is (arg1=bp+4)	
proc drawMidKey
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+4]     ; dx tells us the x coord

		push [effectiveColor] ; color
		push smallHeight ; keyHeight
		push smallWidth ; keyWidth
		push smallY ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		add dx, smallWidth

		; mid of mid part size
		mov ax, keyWidth
		sub ax, smallWidth  

		push [effectiveColor] ; color
		push keyHeight ; keyHeight
		push ax ; keyWidth
		push 1 ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		add dx, ax

		push [effectiveColor] ; color
		push smallHeight ; keyHeight
		push smallWidth ; keyWidth
		push smallY ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		popa
		pop	bp
		ret
endp drawMidKey



		
; draw a piano blackKey. *MID* x position is (arg1=bp+4)	
proc drawBlackKey
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+4]     ; dx tells us the x coord
		sub dx, smallWidth
		dec dx

		; the black keys width is 2*smallWidth + 1
		mov ax, smallWidth
		shl ax, 1
		inc ax

		; black key height is SCREEN_HEIGHT-keyHeight - 1
		mov bx, 200
		sub bx, smallHeight
		dec bx
		
		push [effectiveColor] ; color
		push bx ; keyHeight
		push ax ; keyWidth
		push 1 ; y
		push dx ; x

		call rectangle

		add sp, 10 ; clear stack

		popa
		pop	bp
		ret

endp drawBlackKey


proc drawKeySelect
		push bp
		mov bp, sp
		pusha

		mov dx, [bp+4]     ; dx tells us the x coord
		mov ax, [bp+6]     ; ax tells us the key

		push dx

		cmp ax, mid
		je drawMid
		cmp ax, left
		je drawLeft
		cmp ax, right
		je drawRight
		cmp ax, blackKey
		je drawBlack
		
	drawMid:
		call drawMidKey
		jmp finishKey
	drawRight:
		call drawRightKey
		jmp finishKey
	drawLeft:
		call drawLeftKey
		jmp finishKey
	drawBlack:
		call drawBlackKey
		jmp finishKey
	finishKey:
		pop dx

		popa
		pop	bp
		ret
endp drawKeySelect


proc drawPiano
		push bp
		mov bp, sp
		pusha

		; init vars
		xor cx, cx
		mov bx, offset keysArr
		mov dx, initX ; initial x position
	
	keyLoop:	
		mov ax, [bx]

		cmp ax, blackKey
		je blackDraw
		jmp whiteDraw

	blackDraw:
		mov [effectiveColor], cyan
		jmp generalDraw
	
	whiteDraw:
		mov [effectiveColor], white

	generalDraw:
		push ax
		push dx
		call drawKeySelect
		add sp, 4 ; pop items

		push ax

		mov ax, cx
		shl ax, 1

		; each item in the array is 1 word=2 bytes long
		mov bx, offset keysLocs
		add bx, ax
		mov [bx], dx

		pop ax

		; add bx, 2 ; each item in the array is 1 word=2 bytes long
		inc cx

		; check if we should increment the dx
		cmp ax, blackKey
		je postInc
		
		add dx, keyWidth
		add dx, smallWidth
		add dx, 2 ; was 2. testing

	postInc:
		mov ax, cx
		shl ax, 1

		; each item in the array is 1 word=2 bytes long
		mov bx, offset keysArr
		add bx, ax  ; used at the beggining of the loop

		cmp cx, keyCount-1 ; was keyCount. testing
		jle keyLoop

		popa
		pop	bp
		ret
endp drawPiano


; color k-th key. k is (arg1=bp+4)
proc colorKey
		push bp
		mov bp, sp
		pusha

		mov ax, [bp+4]     ; ax tells us the key
		shl ax, 1
		
		; get the location of the key
		mov bx, offset keysLocs
		add bx, ax
		mov dx, [bx]

		; get the type of the key
		mov bx, offset keysArr
		add bx, ax
		mov ax, [bx]

		; draw the key
		push ax
		push dx
		call drawKeySelect
		add sp, 4 ; pop items

		popa
		pop	bp
		ret
endp colorKey


; first parameter is the frequency, 
; which is given by: (PC TIMER TICK RATE) / (NOTE FREQUENCY)
; reference used: https://stackoverflow.com/questions/61670296/generating-music-tone-using-pc-speaker
PROC playSound
        push bp
        mov bp, sp
        pusha

        mov al, 182         
        out 43h, al          

        mov ax, [bp+4]       ; frequency
        
		; write the frequency to 42h
        out 42h, al        
        mov al, ah          
        out 42h, al 

		; ask the Programmable Peripheral Interface to connect speaker to the square-wave
		; generator, by setting the two least significat bits of PPI port 0x61
        in al, 61h         
        or al, 00000011b   
        out 61h, al        
        mov bx, 15         

		; wait to let the sound play
        call delay
        call delay

		; set back to zero the two least significant bits of PPI port 0x61
        in  al, 61h         
        and al, 11111100b   
        out 61h, al   

        pop bp
        popa      

        RET
endp playSound

; delay for a cx,dx microseconds (030d40h = 200000)
PROC delay
        pusha
        mov cx, 03h
        mov dx, 0d40h
        mov al, 0
        mov ah, 86h
        int 15h
        popa
        RET
endp delay


; check which frequency should be used for the key (index stored in currKey)
; proc assumes that currKey is set to a valid index
PROC getFreq
		push ax
		push bx

		mov bx, offset freqs
		mov ax, [currKey]
		shl ax, 1
		add bx, ax
		mov ax, [bx]
		mov [currFreq], ax

		pop bx
		pop ax
		ret
endp getFreq

; check which x value should be used for the key (index stored in currKey)
; proc assumes that currKey is set to a valid index
PROC getX
		push ax
		push bx		

		mov bx, offset keysLocs
		mov ax, [currKey]
		shl ax, 1
		add bx, ax
		mov ax, [bx]
		mov [currPos], ax

		pop bx
		pop ax
		ret
endp getX



; check which color should be used for the key (index stored in currKey)
; proc assumes that currKey is set to a valid index
PROC getColor
		push ax
		push bx

		mov bx, offset keysArr
		mov ax, [currKey]
		shl ax, 1
		add bx, ax

		mov [currColor], white  ; default color
		mov ax, blackKey
		cmp [bx], ax  ; check if color should be set to black black
		je setColorBlack
		jmp returnColor

	setColorBlack:
		mov [currColor], cyan  ; black is currently cyan because its more fun!

	returnColor:
		pop bx
		pop ax
		ret
endp getColor


; convert key to index of the key. key will be stored in var currKey
; at the same time, set the appropriate frequency and color
PROC findKey
		push bp
		mov bp, sp
		pusha

		mov ax, [bp+4]     ; ax tells us the key
		mov bx, offset keyBoard
		xor cx, cx

		; while the key is not found, increment bx, until we find the key or we reach the end of the array

	keyFindLoop:
		mov dl, [byte ptr bx]
		cmp al, dl
		je keyFound
		inc bx
		inc cx

		cmp cx, keyCount
		je keyNotFound
		jmp keyFindLoop

	keyFound:
		mov [currKey], cx
		
		call getColor  ; set the color needed
		call getFreq   ; set the frequency needed
		call getX 	   ; set the x value needed
		
		jmp returnKey

	keyNotFound:
		mov [currKey], -1

	returnKey:
		popa
		pop	bp
		ret
endp findKey


; play sound and create a visual effect, assuming that the following are valid and set:
; currFreq, currColor, currKey
PROC playNote
		push bp
		mov bp, sp
		pusha

		mov [effectiveColor], yellow  ; set color
		
		push [currKey]
		call colorKey
		add sp, 2  ; pop

		push [currFreq]
		call playSound
		add sp, 2  ; pop

		mov ax, [currColor]
		mov [effectiveColor], ax  ; set color back to original
		
		push [currKey]
		call colorKey
		add sp, 2  ; pop

		popa
		pop bp
		ret
endp playNote


proc playPiano
		push bp
		mov bp, sp
		pusha

	readingLoop:

	charReadyLoop:
		mov ah, 0bh
		int 21h
		cmp al, 0
		je charReadyLoop

		; get the key
		mov ah, 0h
		int 16h

		; convert key to index
		xor ah, ah ; get rid of the scan code, to keep only the ascii value

		cmp al, '3'
		je finishPlay

		cmp al, '2'
		je helper

		push ax
		call findKey
		add sp, 2  ; pop

		; if the key is valid, play the note
		cmp [currKey], -1
		je keyNotValid

		; play the note
		call playNote
	
	keyNotValid:
		jmp readingLoop

	helper:
		call presentHelper
		jmp readingLoop		

	finishPlay:
		popa
		pop bp
		ret
endp playPiano


;================================
; to show .bmp files

proc OpenFile
	; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, offset filename
	int 21h
	jc openerror
	mov [filehandle], ax
	ret
openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp OpenFile


proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader 

proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette


proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB .
	mov al,[si+2] ; Get red value .
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it .
	mov al,[si+1] ; Get green value .
	shr al,2
	out dx,al ; Send it .
	mov al,[si] ; Get blue value .
	shr al,2
	out dx,al ; Send it .
	add si,4 ; Point to next color .
	; (There is a null chr. after every color.)
	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine

	rep movsb ; Copy line to the screen
			;rep movsb is same as the following code :
			;mov es:di, ds:si
			;inc si
			;inc di
			;dec cx
			; ... ;loop until cx=0
	pop cx
	loop PrintBMPLoop
ret
endp CopyBitmap


proc presentHelper
	; Process BMP file
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap

	; wait for key to exit
	charReadyLoop2:
		mov ah, 0bh
		int 21h
		cmp al, 0
		je charReadyLoop2

		; get the key
		mov ah, 0h
		int 16h

		; convert key to index
		xor ah, ah ; get rid of the scan code, to keep only the ascii value

		cmp al, '3'
		je finishPlay2
		jmp backToPlay

	finishPlay2:
		; back to text mode
		mov ah, 0
		mov al, 2
		int 10h

		; exit program
		mov ax, 4c00h
		int 21h

	backToPlay:
		; setting the mode to display mode clears the screen!
		mov ah, 0   ; set display mode function.
		mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
		int 10h     ; set it!

		call drawPiano
		ret
endp presentHelper


;================================



start:
	mov ax, @data
	mov ds, ax

	mov ah, 0   ; set display mode function.
	mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
	int 10h     ; set it!

	; Process BMP file
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	; Wait for key press
	mov ah,1
	int 21h

	; setting the mode to display mode clears the screen!
	mov ah, 0   ; set display mode function.
	mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
	int 10h     ; set it!

	call drawPiano
	
	call playPiano

	; back to text mode
	mov ah, 0
	mov al, 2
	int 10h



exit:
	; exit program
	mov ax, 4c00h
	int 21h
END start




	; mov ah, 0eh           ;0eh = 14
	; mov al, 'c'
	; xor bx, bx            ;Page number zero
	; mov bl, 0ch           ;Color is red
	; int 10h

	; mov  dl, 20   ;Column
	; mov  dh, 12   ;Row
	; mov  bh, 0    ;Display page
	; mov  ah, 02h  ;SetCursorPosition
	; int  10h

	; mov  al, 'x'
	; inc al
	; mov  bl, 0Ch  ;Color is red
	; mov  bh, 0    ;Display page
	; mov  ah, 0Eh  ;Teletype
	; int  10h