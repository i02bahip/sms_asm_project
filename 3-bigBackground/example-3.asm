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
.define SCREEN_HEIGHT_TILES 24
.define TEST 6
.define BG_WIDTH 160*2
.define BG_FULL 160*24
.define LULU 8*2
.define ROW_OF_SCREEN 32*2
.define VISIBLE_PART_OF_SCREEN 32*24*2
.define VDP_HORIZONTAL_SCROLL_REGISTER 8
.define SCROLL_HORIZONTAL_SPEED 1
.define STACK_INIT_ADDRESS $dff0
.define PLAYER1_JOYSTICK_DOWN 1
.define PLAYER1_JOYSTICK_UP 0
.define PLAYER1_JOYSTICK_RIGHT 3
.define PLAYER1_JOYSTICK_LEFT 2
.define PLAYER1_BUTTON_A 4
.define PLAYER1_BUTTON_B 5
;---------- VDP REGISTER 0 CONSTANTS
; Bit 7: Vertical scroll lock
; Bit 6: Horizontal scroll lock
; Bit 5: Hide leftmost 8 pixels
; Bit 4: Enable line interrupts
; Bit 3: Shift sprites left 8 pixels
; Bit 2: Mode 4 enable
; Bit 1: extra height enable
; Bit 0: Sync enable
.define VDP_REG_0_TURN_SCREEN_ON_FULL     %00100110 
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
    VDPStatus db        ; Gets updated by the frame int. handler.
    Scroll db           ; Vertical scroll register mirror.
    RealScroll db
    BgScroll dw
    TileMapIndex dw
    TileMapAddressIndex dw
    BgWidth dw
    Controller db
    ScrollHorizontalSpeed db
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
   call GetCtrl
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
    ;----------------------------------------

    call InitTileMapIndex
    ld hl,BG_WIDTH
    ld (BgWidth),hl
    ; Set the background
    call setScreen
    call InitTileMapIndex

    ld a,0
    ld (RealScroll),a
    ld (ScrollHorizontalSpeed),a
    ld hl,ROW_OF_SCREEN
    ld (BgScroll),hl

    ; Turn on the screen
    call TurnOnscreen

Loop:
    call WaitForFrameInterrupt
    call CopyScrollBlock
    call MoveScroll  ; Update the scroll.
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister
    ;ld a,0
    ;ld (RealScroll),a
    ;ld (ScrollHorizontalSpeed),a
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

InitTileMapIndex:
    ld hl,TileMap
    ld (TileMapIndex),hl
    ld hl,TILEMAP_ADDRESS
    ld (TileMapAddressIndex),hl
    ret

MoveScroll:        
    ld a,(Scroll)
    sub SCROLL_HORIZONTAL_SPEED
    jp nz,ResetRealScroll
    ld (RealScroll),a
ResetRealScroll:
    ld (Scroll),a
    ret

setScreen:
    ld a,SCREEN_HEIGHT_TILES    ; register a is now Rowcount
setScreenLoop:
    push af ; save the rowcount

    ld hl,(TileMapAddressIndex)
    PrepareVram
    ; 2. Output tilemap data
    ld hl,(TileMapIndex)
    ld bc,ROW_OF_SCREEN  ; Counter for number of bytes to write
    call LoadVRAM

    ;pointing new row in full BG
    ld hl,(TileMapIndex)
    ld bc,(BgWidth)    ; DE = A
    ;ld d,0
    add hl,bc
    ld (TileMapIndex),hl

    ;pointing new row in screen
    ld hl,(TileMapAddressIndex)
    ld e,ROW_OF_SCREEN    ; DE = A
    ld d,0
    add hl,de
    ld (TileMapAddressIndex),hl

    pop af ; Get the rowcount
    sub 1
    jp nz,setScreenLoop
    ret

CopyScrollBlock:
    ld a,(Scroll)
    and %111
    jp nz,DontCopyBlocks
    call CopyBlocks
    ld a,(RealScroll)
    add 2
    ld (RealScroll),a
    ld ix,(BgScroll)
    inc ix
    inc ix
    ld (BgScroll),ix

;Si llega al final de la longitud del mapa en memoria, empieza de nuevo
    or a ;clear carry flag
    ld hl,(BgScroll)
    ld de,BG_WIDTH
    sbc hl,de
    jr nc,ResetBgSroll   ;BgScroll >= BG_WIDTH
    ret ;BG_WIDTH > BgScroll
ResetBgSroll:
    ld e,0
    ld d,0
    ld (BgScroll),de
    ret
DontCopyBlocks:
    ret

CopyBlocks:
    call InitTileMapIndex
    ld hl,(TileMapAddressIndex)
    ld a,(RealScroll)
    ld e,a
    ld d,0
    add hl,de   ; Añade offset del scroll real para que copie en la posición correcta en vram los bloques del mapa completo
    ld (TileMapAddressIndex),hl
    ld a,SCREEN_HEIGHT_TILES    ; Contador de lineas vertical
CopyBlocksLoop:
    push af ; Salvar el contador
    ld hl,(TileMapAddressIndex)
    PrepareVram

    ; Copiamos el tile que corresponde del mapa completo
    ld hl,(TileMapIndex)
    ld de,(BgScroll) ; Añade el offset del puntero hacia el mapa completo
    add hl,de
    ld bc,2  ; Counter for number of bytes to write
    call LoadVRAM

    ; Pasamos a apuntar a la siguiente fila del mapa completo
    ld hl,(TileMapIndex)
    ld bc,(BgWidth)
    add hl,bc
    ld (TileMapIndex),hl

    ; Apuntamos a la siguiente fila del mapa en la vram
    ld hl,(TileMapAddressIndex)
    ld e,ROW_OF_SCREEN    ; DE = A
    ld d,0
    add hl,de
    ld (TileMapAddressIndex),hl

    pop af ; Obtener el contador
    sub 1
    jp nz,CopyBlocksLoop
    ret

GetCtrl:
    push af
    in a,$dc
    ld [Controller],a
    pop af
    ret
ButtonHandle:
    push af
    ld a,[Controller]
    ;bit PLAYER1_JOYSTICK_UP,a
    ;call z, _MoveUp
    ;bit PLAYER1_JOYSTICK_DOWN,a
    ;call z, _MoveDo
    bit PLAYER1_JOYSTICK_LEFT,a
    call z, _MoveLe
    bit PLAYER1_JOYSTICK_RIGHT,a
    call z, _MoveRi
    pop af
    ret
_MoveLe:
    ret
_MoveRi:
    ;ld a,SCROLL_HORIZONTAL_SPEED
    ;ld (ScrollHorizontalSpeed),a
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

; TileMap
TileMap:
   .include "inc/art/tileMaps/bgTileMap.inc"
TileMapEnd:

;Palettes
BGPaletteData:
    .include "inc/art/palettes/bgPalette.inc"
SpritePaletteData:
    .include "inc/art/palettes/spritePalette.inc"

;tile set
TileSet:
    .include "inc/art/tileSets/bgTileSet.inc"
TileSetEnd: