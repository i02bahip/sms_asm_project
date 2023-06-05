;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 1.2,"Background example","Master system assembler tutorial","PBH"

;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
    defaultslot 0
    slotsize $8000
    slot 0 $0000          ; ROM
    slotsize $2000
    slot 1 $c000          ; RAM
.endme

.rombankmap
    bankstotal 2
    banksize $8000
    banks 2
.endro

; =============================================================================
; M A C R O S
; =============================================================================
.macro PrepareVram
   rst $20
.endm

; =============================================================================
; C O N S T A N T S
; =============================================================================
.define VDP_CONTROL $bf
.define ONE_PALETTE_SIZE 16
.define PALETTE_1_ADDRESS $c000 ; Bank 1 address.
.define PALETTE_2_ADDRESS $c010 ; Bank 2 address.
.define ADDRESS_OF_FIRST_TILE $0000
.define TILEMAP_ADDRESS $3800
.define VISIBLE_PART_OF_SCREEN 32*24*2
.define VDP_HORIZONTAL_SCROLL_REGISTER 8
.define SCROLL_HORIZONTAL_SPEED 1
.define STACK_INIT_ADDRESS $dff0

;---------- VDP REGISTER 0 CONSTANTS
; Bit 7: Vertical scroll lock
; Bit 6: Horizontal scroll lock
; Bit 5: Hide leftmost 8 pixels
; Bit 4: Enable line interrupts
; Bit 3: Shift sprites left 8 pixels
; Bit 2: Mode 4 enable
; Bit 1: extra height enable
; Bit 0: Sync enable
.define VDP_REG_0_TURN_SCREEN_ON_FULL     %00110110 
.define VDP_REGISTER_0_INDEX 0

;---------- VDP REGISTER 1 CONSTANTS
; Bit 7: unused
; Bit 6: VBlank interrupts
; Bit 5: Enable display
; Bit 4: 28 row/224 line mode
; Bit 3: 30 row/240 line mode
; Bit 2: unused
; Bit 1: Doubled sprites -> 2 tiles per sprite, 8x16
; Bit 0: Zoomed sprites -> 16x16 pixels
.define VDP_REG_1_TURN_SCREEN_ON_TALL_SPRITES %11100010
.define VDP_REGISTER_1_INDEX 1

;======================================================================
; V A R I A B L E S
; =============================================================================
.ramsection "variables" slot 1
    VDPStatus db          ; Gets updated by the frame int. handler.
    Scroll db             ; Vertical scroll register mirror.
.ends


; =============================================================================
; L I B R A R I E S
; =============================================================================
.include "inc/support/stdlib.inc" ; General/supporting routines.

.bank 0 slot 0
.org $0000
;==============================================================
; Boot section
;==============================================================
di              ; disable interrupts
im 1            ; Interrupt mode 1
ld sp,STACK_INIT_ADDRESS
jp main         ; jump to main program

;==============================================================
.org $0020               ; rst $20: Prepare vram at address in HL.
   ld a,l                ; Refer to the PrepareVram macro.
   out (VDP_CONTROL),a
   ld a,h
   or $40
   out (VDP_CONTROL),a
   ret

.org $0038                 ;Frame interrupt address
   ex af,af'               ; save accumulator in its shadow reg
   exx
   in a,(VDP_CONTROL)        ; get vdp status / satisfy interrupt.
   ld (VDPStatus),a            ; save vdp status in ram
   exx
   ex af,af'               ; restore accumulator.
   ei                  ; enable interrupts.
   reti

.org $0066              ; Pause button interrupt
    retn

;==============================================================
; Main program
;==============================================================
main:
    call ClearRam
    ;==============================================================
    ; Set up VDP registers
    ;==============================================================
    ld hl,VdpInitData
    ld b,VdpInitDataEnd-VdpInitData
    ld c,VDP_CONTROL
    otir

    ;==============================================================
    ; Load palettes
    ;==============================================================
    ;BG Palette
    ld hl,PALETTE_1_ADDRESS  ; Load the BG palette.
    PrepareVram
    ld hl,BGPaletteData
    ld bc,ONE_PALETTE_SIZE
    call LoadVRAM

    ;Sprite palette
    ld hl,PALETTE_2_ADDRESS  ; Load the BG palette.
    PrepareVram
    ld hl,SpritePaletteData
    ld bc,ONE_PALETTE_SIZE
    call LoadVRAM

    ;==============================================================
    ; Load tiles (tileSet)
    ;==============================================================
    ; 1. Set VRAM write address to tile index 0
    ; by outputting $4000 ORed with $0000
    ld hl,ADDRESS_OF_FIRST_TILE
    PrepareVram
    ; 2. Output tile data
    ld hl,TileSet              ; Location of tile data
    ld bc,TileSetEnd-TileSet  ; Counter for number of bytes to write
    call LoadVRAM
    ;Setup scroll
    ld a,0
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister

    ;==============================================================
    ; Write text to name table
    ;==============================================================
    ; 1. Set VRAM write address to name table index 0
    ; by outputting $4000 ORed with $3800+0 
    ld hl,TILEMAP_ADDRESS
    PrepareVram
    ; 2. Output tilemap data
    ld hl,TileMap
    ld bc,VISIBLE_PART_OF_SCREEN  ; Counter for number of bytes to write
    call LoadVRAM

    ; Turn on the screen
    call TurnOnscreen

    ; Infinite loop to stop program

Loop:
    call WaitForFrameInterrupt
    ld a,(Scroll)
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister      ; Load the vertical scroll register.
    call MoveScroll  ; Update the scroll register buffer.
    jp Loop  

TurnOnscreen:
    di
    ld a,VDP_REG_0_TURN_SCREEN_ON_FULL
    ld b,VDP_REGISTER_0_INDEX
    call SetRegister
    ld a,VDP_REG_1_TURN_SCREEN_ON_TALL_SPRITES
    ld b,VDP_REGISTER_1_INDEX
    call SetRegister
    ei
    ret    

MoveScroll:        
    ld a,(Scroll)
    sub SCROLL_HORIZONTAL_SPEED
    ld (Scroll),a
    ret

;==============================================================
; Data
;==============================================================
; VDP initialisation data
VdpInitData:
    .db $24,$80
    .db $00,$81
    .db $ff,$82
    ; REGISTER 3 AND 4 ARE USELESS
    .db $ff,$85
    .db $ff,$86
    .db $f0,$87
    .db $00,$88
    .db $00,$89
    .db $ff,$8a
VdpInitDataEnd:

; Hello world
TileMap:
   .include "inc/art/tileMaps/tileMap-1.asm"
TileMapEnd:

;Palettes
BGPaletteData:
    .include "inc/art/palettes/backgPalette.asm"
SpritePaletteData:
    .include "inc/art/palettes/spritePalette.asm"

;tile set
TileSet:
    .include "inc/art/tileSets/tileSet-1.asm"
TileSetEnd: