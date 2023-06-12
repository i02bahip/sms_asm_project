CopyScrollBlock:
    ld a,(Scroll)
    and %111 ; Se comprueba que el scroll sea multiplo de 8. Si no es así, no copia tiles, pero permite que se pueda volver a copiar en un futuro cuando sea multiplo
    jp nz,DontCopyBlocksButAllow

    ; Si si ha sido multiplo, hay que comprobar que no haya bloqueo de copia de tiles (quiere decir que ya se han copiado. Para casos de estar parado cuando se es multiplo de 8)
    ld a,(ActionStatus)
    bit 3,a
    jp z, DontCopyBlocks

    ld a,(ScrollStatus)
    ; Si hemos llegado aquí, quiere decir que tenemos que copiar bloques
    bit 3,a
    jp z, OnlyCopy
    bit 0,a
    jp z, UpdateAndCopy
    bit 2,a
    ;jp z, CopyAndUpdate
    ;call UpdateAndCopy
    jp z, UpdateAndCopy
    call CopyAndUpdate
CheckBGLimits:
    call NoZeroScroll
    ;------------------------------
    ;call UpdateScrollIndexes
    ;------------------------------

    ; Estamos en el caso de que hay que refrescar la columna de tiles
    ; call CopyBlocks
    ; A la salida de copiar bloques, debemos actualizar los indices:
    ; - IndexScrollScreen: indice que recorre las columnas mostradas en pantalla (0-40)
    ; - IndexBgScroll: Indice que recorre el BG entero  de tamaño BG_WIDTH

        ;TODO Aquí va a estar la wea...

    ;Si llega al final de la longitud del mapa en memoria, empieza de nuevo

    or a ;clear carry flag
    ld hl,(IndexBgScroll)
    ld de,BG_WIDTH
    sbc hl,de
    jr nc,ResetIndexBgScroll   ;IndexBgScroll >= BG_WIDTH
    ret ;BG_WIDTH > IndexBgScroll
ResetIndexBgScroll:
    ld e,0
    ld d,0
    ld (IndexBgScroll),de
    ret
DontCopyBlocksButAllow:
    call AllowCopyBlocks
DontCopyBlocks:
    ret

AllowCopyBlocks:
    ld a,(ActionStatus)
    or a,%00001000
    ld (ActionStatus),a
    ret

NotAllowCopyBlocks:
    ld a,(ActionStatus)
    and a,%11110111
    ld (ActionStatus),a
    ret

CopyBlocks:
    call NotAllowCopyBlocks
    call InitTileMapIndex

    ;Aquí vamos a indicar en el mapa de tiles que se muestran por pantalla, la nueva columna a rellenar 

    ld hl,(TileMapAddressIndex) ; Indice de donde empieza la dirección de la tabla de tiles que se muestra en pantalla
    ld a,(IndexScrollScreen) ; Indice de por donde va el scroll real
    ld e,a
    ld d,0
    add hl,de   ; Añade offset del scroll real para que copie en la posición correcta en vram los bloques del mapa completo en el siguiente paso
    ld (TileMapAddressIndex),hl
    ld a,SCREEN_HEIGHT_TILES    ; Contador de lineas vertical
CopyBlocksLoop:
    push af ; Salvar el contador
    ld hl,(TileMapAddressIndex)
    PrepareVram

    ; Copiamos el tile que corresponde del mapa completo
    ld hl,(TileMapIndex)

    ;TODO ------------------------------------
    ;Meter lógica si es para derecha/izquierda
    ;IndexBGScroll debe apuntar a final o al principio..
    ;-----------------------------------------
    call CalculatePointerBgScroll
    ;-----------------------------------------
    ; Copiamos el tile que corresponde del mapa completo
    ld hl,(TileMapIndex)
    ld de,(PointerBgScroll) ; Añade el offset del puntero hacia el mapa completo
    add hl,de
    ld bc,2  ; Counter for number of bytes to write
    call LoadVRAM

    ; Pasamos a apuntar a la siguiente fila del mapa completo
    ld hl,(TileMapIndex)
    ld bc,BG_WIDTH
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

OnlyCopy:
    call CopyBlocks
    ;call UpdateScrollIndexes
    jp CheckBGLimits

CopyAndUpdate:
    call CopyBlocks
    call UpdateScrollIndexes
    jp CheckBGLimits

UpdateAndCopy:
    call UpdateScrollIndexes
    call CopyBlocks
    jp CheckBGLimits
