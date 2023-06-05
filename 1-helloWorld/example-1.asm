.define vdpControl $bf
.define vdpData    $be
;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $8000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 1.2,"Hello World!","SMS programming tutorial program","Maxim"

.bank 0 slot 0
.org $0000
;==============================================================
; Boot section
;==============================================================
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    jp main         ; jump to main program

.org $0066
;==============================================================
; Pause button handler
;==============================================================
    ; Do nothing
    retn

;==============================================================
; Main program
;==============================================================
main:
    ld sp, $dff0

    ;==============================================================
    ; Set up VDP registers
    ;==============================================================
    ld hl,VdpInitData
    ld b,VdpInitDataEnd-VdpInitData
    ld c,vdpControl
    otir

    ;==============================================================
    ; Clear VRAM
    ;==============================================================
    ; 1. Set VRAM write address to 0 by outputting $4000 ORed with $0000
    ld a,$00
    out (vdpControl),a
    ld a,$40
    out (vdpControl),a
    ; 2. Output 16KB of zeroes
    ld bc, $4000    ; Counter for 16KB of VRAM
    ClearVRAMLoop:
        ld a,$00    ; Value to write
        out (vdpData),a ; Output to VRAM address, which is auto-incremented after each write
        dec bc
        ld a,b
        or c
        jp nz,ClearVRAMLoop

    ;==============================================================
    ; Load palette
    ;==============================================================
    ; 1. Set VRAM write address to CRAM (palette) address 0 (for palette index 0)
    ; by outputting $c000 ORed with $0000
    ld a,$00
    out (vdpControl),a
    ld a,$c0
    out (vdpControl),a
    ; 2. Output colour data
    ld hl,PaletteData
    ld b,(PaletteDataEnd-PaletteData)
    ld c,vdpData
    otir

    ;==============================================================
    ; Load tiles (font)
    ;==============================================================
    ; 1. Set VRAM write address to tile index 0
    ; by outputting $4000 ORed with $0000
    ld a,$00
    out (vdpControl),a
    ld a,$40
    out (vdpControl),a
    ; 2. Output tile data
    ld hl,FontData              ; Location of tile data
    ld bc,FontDataEnd-FontData  ; Counter for number of bytes to write
    WriteTilesLoop:
        ; Output data byte then three zeroes, because our tile data is 1 bit
        ; and must be increased to 4 bit
        ld a,(hl)        ; Get data byte
        out (vdpData),a
        inc hl           ; Add one to hl so it points to the next data byte
        dec bc
        ld a,b
        or c
        jp nz,WriteTilesLoop

    ;==============================================================
    ; Write text to name table
    ;==============================================================
    ; 1. Set VRAM write address to name table index 0
    ; by outputting $4000 ORed with $3800+0
    ld a,$00
    out (vdpControl),a
    ld a,$38|$40
    out (vdpControl),a
    ; 2. Output tilemap data
    ld hl,Message
    ld bc,MessageEnd-Message  ; Counter for number of bytes to write
    WriteTextLoop:
        ld a,(hl)    ; Get data byte
        out (vdpData),a
        inc hl       ; Point to next letter
        dec bc
        ld a,b
        or c
        jp nz,WriteTextLoop

    ; Turn screen on
    ld a,%11000000
;          |||| |`- Zoomed sprites -> 16x16 pixels
;          |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (vdpControl),a
    ld a,$81
    out (vdpControl),a

    ; Infinite loop to stop program
Loop:
     jp Loop

;==============================================================
; Data
;==============================================================

; Hello world
Message:
    .dw $28,$45,$4c,$4c,$4f,$00,$37,$4f,$52,$4c,$44,$01
MessageEnd:

;Palettes
PaletteData:
    .include "inc/art/palettes/backgPalette.asm"
PaletteDataEnd:

; VDP initialisation data
VdpInitData:
    .db $04,$80,$00,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VdpInitDataEnd:

;Font
FontData:
    .include "inc/art/fonts/font.asm"
FontDataEnd: