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
.define TILEMAP_ADDRESS_INIT $3802
.define SCREEN_HEIGHT_TILES 24
.define TEST 6
.define BG_WIDTH 160*2
.define BG_FULL 160*24
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
    Scroll db           ; Scroll horizontal: va de forma descendente de FF a 00 de 1 en 1
    IndexScrollScreen db       ; Indice de Scroll por tiles en pantalla: va de forma ascendente de 00 a 40 de dos en dos (2 bytes  = tile). Indica el tile a pintar
    IndexBgScroll dw           ; Indice de Scroll por tiles en el background entero.
    PointerBgScroll dw         ; Apuntará al final del bgscroll o a bgScroll - tamaño de pantalla dependiendo si vamos a derecha o izquierda
    TileMapIndex dw         ;Dirección del tilemap en memoria
    TileMapAddressIndex dw ;Dirección de la tabla que define lo que se ve en la pantalla
    Controller db
    ScrollStatus db     ;TODO aqui guardaremos flags:
    ; Bit 7: Limite inferior
    ; Bit 6: Limite superior
    ; Bit 5: Límite derecho
    ; Bit 4: Límite izquierdo
    ; Bit 3: scroll zero
    ; Bit 2: Dirección derecha
    ; Bit 1: Dirección arriba 
    ; Bit 0: Dirección izquierda
    ActionStatus db     ;TODO aqui guardaremos flags:
    ; Bit 7: Limite inferior
    ; Bit 6: Limite superior
    ; Bit 5: Límite derecho
    ; Bit 4: Límite izquierdo
    ; Bit 3: Copiar bloques
    ; Bit 2: Dirección derecha
    ; Bit 1: Dirección arriba 
    ; Bit 0: Dirección izquierda

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
   in a,VDP_CONTROL        ; get vdp status / satisfy interrupt.
   ld (VDPStatus),a            ; save vdp status in ram
   exx
   ex af,af'               ; restore accumulator.
   call ButtonHandle
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
    ; Init scrollStatus and actionStatus byte
    ;==============================================================
    ld a,0
    or a,%11111111
    ld (ScrollStatus),a
    ld (ActionStatus),a

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
    ; Init InitTileMapIndex
    ;==============================================================
    call setScreen
    call InitTileMapIndex

    ;==============================================================
    ; Init IndexScrollScreen & IndexBgScroll
    ;==============================================================
    ld a,0
    ld (IndexScrollScreen),a
    ld hl,ROW_OF_SCREEN
    ld (IndexBgScroll),hl

    ;==============================================================
    ; Turn on the screen
    ;==============================================================
    call TurnOnscreen

Loop:
    call WaitForFrameInterrupt
    call SetScrollStatus ; Updateamos el registro que indica la direccion de movimiento
    ; registros modificados:
    ; - ScrollStatus
    call MoveScroll  ; Update the scroll.
    ; registros modificados:
    ; - Scroll
    ; - IndexScrollScreen (lo pone a 0 si hemos llegado al final del scroll)
    call CopyScrollBlock
    ; registros modificados:
    ; - TODO
    ld a,(Scroll)
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister
    jp Loop  
    ret

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

setScreen:
    call InitTileMapIndexInVisibleColumns
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
    ld bc,BG_WIDTH    ; DE = A
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

CalculatePointerBgScroll:
    ld hl,(IndexBgScroll)
    ld a,(ScrollStatus)
    bit 0,a
    call z, SetPointerLeft
    bit 2,a
    call z, SetPointerRight
    ld (PointerBgScroll), hl
    ret

SetPointerRight:
    ret

SetPointerLeft:
    ld e,ROW_OF_SCREEN    ; DE = A
    ld d,0
    sbc hl,de
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
.include "inc/support/tileFunctions.asm" ; General/supporting routines.
.include "inc/support/copyBlockFunctions.asm"
.include "inc/support/scrollFunctions.asm"