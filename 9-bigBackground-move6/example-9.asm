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
.define BG_TILES_WIDTH 128*2
.define BG_WIDTH 160*2
.define SCREEN_WIDTH 32*2
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

.define VDP_REG_7_BG_COLOR     %00000000 
.define VDP_REGISTER_7_INDEX 7

.define VDP_REG_10_LINE_INTERRUPT     %00000000 
.define VDP_REGISTER_10_INDEX $0A

;======================================================================
; V A R I A B L E S
; =============================================================================
.ramsection "variables" slot 1
    VDPStatus db        ; Gets updated by the frame int. handler.
    Scroll db           ; Scroll horizontal: va de forma descendente de FF a 00 de 1 en 1
    LastScroll db
    RealScrollScreen db ; Indice de Scroll por tiles en pantalla: va de forma ascendente de 00 a 40 de dos en dos (2 bytes  = tile). Indica el tile a pintar
    IndexBgScroll dw           ; Indice de Scroll por tiles en el background entero. Va de forma ascendente de 00 a .
    PointerBgScroll dw         ; Apuntará al final del bgscroll o a bgScroll - tamaño de pantalla dependiendo si vamos a derecha o izquierda
    MapMemoryIndex dw         ;Dirección del tilemap en memoria
    MapVRAMIndex dw ;Dirección de la tabla que define lo que se ve en la pantalla
    CopyMapVRAMIndex dw
    CopyMapMemoryIndex dw
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
    ; Bit 7: -
    ; Bit 6: -
    ; Bit 5: -
    ; Bit 4: Bloques copiados
    ; Bit 3: Copiar bloques
    ; Bit 2: - Ultima direccion derecha
    ; Bit 1: -
    ; Bit 0: - Ultima direccion izquierda
    BgColor db

.ends

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
   and	%10000000						; VDP Interrupt information.
        ;x.......						; 1: VBLANK Interrupt, 0: H-Line Interrupt [if enabled].
        ;.X.....						; 9 sprites on a raster line.
        ;..x.....						; Sprite Collision.
   jp		nz, _else
   call	HBlank_Handler
   jp		_endif
_else:
   call	VBlank_Handler
_endif:
   exx
   ex af,af'               ; restore accumulator.
   ei                  ; enable interrupts.
   reti

.org $0066              ; Pause button interrupt
    retn

VBlank_Handler:
    call ButtonHandle
    call GetCtrl
    ret


HBlank_Handler:
    ;ld a,(BgColor)

    ld a,(BgColor)
    add 1
    ld (BgColor),a

    ld b,VDP_REGISTER_7_INDEX
    call SetRegister
	ret
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
    xor a
    ld (BgColor), a
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
    xor a
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister

;==============================================================
; Set init values
;==============================================================
    call setScreen
    call InitVRAMAndMemoryIndex

;==============================================================
; Turn on the screen
;==============================================================
    call TurnOnscreen
;==============================================================
; MAIN LOOP
;==============================================================
Loop:
    call ResetBGColor
    call WaitForFrameInterrupt ; Esperamos a que se haya pintado la pantalla.
    call SetRegister
    call ChangeBGColor
    call UpdateScrollStatus ;Updateamos los flags que indican direccion de scroll. Modifica: ScrollStatus db
    call ChangeBGColor
    call UpdateScroll  ;Updateamos la variable scroll. Modifica: Scroll db
    call ChangeBGColor
    call UpdateScrollIndexes ;Updateamos los indices del scroll. Modifica: PointerBgScroll, RealScrollScreen, IndexBgScroll, ActionStatus, ScrollStatus
    call ChangeBGColor
    call MoveScrollRegister ;Una vez todo esté listo, se mueve de forma real el scroll.
    call ChangeBGColor
    jp Loop  
    ret

;==============================================================
;----- 0) INIT FUNCTIONS ----------------------------
;==============================================================
setScreen:
    call InitVRAMAndMemoryIndex
    ld a,SCREEN_HEIGHT_TILES    ; register a is now Rowcount
setScreenLoop:
    push af ; save the rowcount

    ld hl,(MapVRAMIndex)
    PrepareVram
    ; 2. Output tilemap data
    ld hl,(MapMemoryIndex)
    ld bc,SCREEN_WIDTH  ; Counter for number of bytes to write
    call LoadVRAM

    ;pointing new row in full BG
    ld hl,(MapMemoryIndex)
    ld bc,BG_WIDTH    ; DE = A
    ;ld d,0
    add hl,bc
    ld (MapMemoryIndex),hl

    ;pointing new row in screen
    ld hl,(MapVRAMIndex)
    ld e,SCREEN_WIDTH    ; DE = A
    ld d,0
    add hl,de
    ld (MapVRAMIndex),hl

    pop af ; Get the rowcount
    sub 1
    jp nz,setScreenLoop
    ret

InitVRAMAndMemoryIndex:
    ld hl,TileMap
    ld (MapMemoryIndex),hl ;Apunta al inicio del mapa completo en memoria
    ld hl,TILEMAP_ADDRESS
    ld (MapVRAMIndex),hl ;Apunta al inicio del mapa en VRAM
    ret

TurnOnscreen:
    di
    ld a,VDP_REG_0_TURN_SCREEN_ON_FULL
    ld b,VDP_REGISTER_0_INDEX
    call SetRegister
    ld a,VDP_REG_1_TURN_SCREEN_ON_TALL_SPRITES
    ld b,VDP_REGISTER_1_INDEX
    call SetRegister
    ld a,VDP_REG_10_LINE_INTERRUPT
    ld b,VDP_REGISTER_10_INDEX
    call SetRegister
    ld a,VDP_REG_7_BG_COLOR
    ld b,VDP_REGISTER_7_INDEX
    call SetRegister
    ei
    ret    

;==============================================================
;----- 1) UPDATE SCROLL STATUS ----------------------------
;==============================================================
UpdateScrollStatus:
    call ScrollNoMovement
    ld a,(Controller)
    bit PLAYER1_JOYSTICK_LEFT,a
    jp z,ScrollDirectionLeft
    bit PLAYER1_JOYSTICK_RIGHT,a
    jp z,ScrollDirectionRight
ContinueScrollStatus:
    ret

;==============================================================
;----- 2) UPDATE SCROLL ----------------------------
;==============================================================
UpdateScroll:
    ld a,(ScrollStatus)
    bit 0,a
    jp z,updateScrollLeft
    bit 2,a
    jp z,updateScrollRight
ContinueScroll:
    ret

updateScrollRight:
    ld a,(ActionStatus)
    bit 0,a
    call z,SetDirChanged
    ld a,(ActionStatus)
    bit 0,a
    call nz,UnsetDirChanged
    ld a,(Scroll)
    ld (LastScroll), a
    sub SCROLL_HORIZONTAL_SPEED
    ld (Scroll),a
    jp ContinueScroll

updateScrollLeft:
    ld a,(ActionStatus)
    bit 2,a
    call z,SetDirChanged
    ld a,(ActionStatus)
    bit 2,a
    call nz,UnsetDirChanged
    ld a,(Scroll)
    ld (LastScroll), a
    add SCROLL_HORIZONTAL_SPEED
    ld (Scroll),a
    jp ContinueScroll

TestDebug:
    ret

;==============================================================
;----- 3) UPDATE SCROLL INDEXES ----------------------------
;==============================================================
UpdateScrollIndexes:
    ld a,(Scroll)
    and %111 ; Se comprueba que el scroll sea multiplo de 8. Si si lo es, calculamos y copiamos bloques
    call z, TestDebug

    ld a,(LastScroll)
    and %111 ; Se comprueba que el scroll sea multiplo de 8. Si si lo es, calculamos y copiamos bloques
    jp nz, NoUpdateIndex

    ld a,(ActionStatus) ;Si se han copiado ya los bloques, nos saltamos copiarlos otra vez, en el caso que nos hayamos quedado parados en scroll multiplo de 8 
    bit 4,a
    jp z, NoUpdateIndex
    ld a,(LastScroll)
    add 1
    sub 1
    jp z, dontAdd
    cpl
    SRL a
    SRL a
    SRL a
    add 1
    jp z, dontAdd
    SLA a
dontAdd:

    ld (RealScrollScreen), a

ContinueCopying:
    call CalculatePointerBgScroll
    call PrepareCopyVars
    call CopyBlocks
    call UpdateIndexBGScroll

NoUpdateIndex
    ret

UpdateIndexBGScroll:
    ld a,(ScrollStatus)
    bit 0,a
    jp z, moveIndexesLeft
    bit 2,a
    jp z, moveIndexesRight
ContinueUpdating:
    ret

moveIndexesRight:
    ld a,(ActionStatus)
    bit 1,a
    jp z, ContinueUpdating
    ld ix,(IndexBgScroll)
    inc ix
    inc ix
    ld (IndexBgScroll),ix
    jp ContinueUpdating

moveIndexesLeft:
    ld a,(ActionStatus)
    bit 1,a
    jp z, ContinueUpdating
    ld ix,(IndexBgScroll)
    dec ix
    dec ix
    ld (IndexBgScroll),ix
    jp ContinueUpdating

CalculatePointerBgScroll:
    ld hl,(IndexBgScroll)
    ld a,(ScrollStatus)
    bit 0,a
    jp z, SetPointerLeft
    jp SetPointerRight
ContinueCalculate:
    ld (PointerBgScroll), hl
    ret

SetPointerRight:
    ld e,SCREEN_WIDTH
    ld d,0
    add hl,de
    ld a,(ActionStatus)
    bit 1,a
    jp nz, ContinueCalculate
    ld e,2    ; DE = A
    ld d,0
    sbc hl,de
    jp ContinueCalculate

SetPointerLeft:
    ld a,(ActionStatus)
    bit 1,a
    jp z, ContinueCalculate
    ld e,2    ; DE = A
    ld d,0
    sbc hl,de
    jp ContinueCalculate

PrepareCopyVars:
    ;Aquí vamos a indicar en el mapa de tiles que se muestran por pantalla, la nueva columna a rellenar 
    ld hl,(MapVRAMIndex) ; Indice de donde empieza la dirección de la tabla de tiles que se muestra en pantalla
    ld a,(RealScrollScreen) ; Indice de por donde va el scroll real (columna invisible)
    ld e,a
    ld d,0
    add hl,de   ; Añade offset del scroll real para que copie en la posición correcta en vram los bloques del mapa completo en el siguiente paso
    ld (CopyMapVRAMIndex),hl
    ld hl,TileMap
    ld (CopyMapMemoryIndex),hl ;Apunta al inicio del mapa completo en memoria

    ret

;==============================================================
;----- 5) MOVE SCROLL REGISTER ----------------------------
;==============================================================
MoveScrollRegister:
    ld a,(Scroll)
    ld b,VDP_HORIZONTAL_SCROLL_REGISTER
    call SetRegister

    ld a,(ScrollStatus)
    bit 0,a
    call z, LastDirectionLeft

    ld a,(ScrollStatus)
    bit 2,a
    call z, LastDirectionRight

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

; =============================================================================
; L I B R A R I E S
; =============================================================================
.include "inc/support/stdlib.inc" ; General/supporting routines.
.include "inc/support/flagHelper.asm"
.include "inc/support/copyBlocksHelper.asm"
.include "inc/support/controlHelper.asm"