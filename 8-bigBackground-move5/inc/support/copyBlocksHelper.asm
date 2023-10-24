CopyBlocks:
    call SetBlocksAlreadyCopied
    call UnsetCopyBlocks
    call InitTileMapIndex
    ;Aquí vamos a indicar en el mapa de tiles que se muestran por pantalla, la nueva columna a rellenar 
    ld hl,(TileMapAddressIndex) ; Indice de donde empieza la dirección de la tabla de tiles que se muestra en pantalla
    ld a,(IndexScrollScreen) ; Indice de por donde va el scroll real (columna invisible)
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
    ld e,ROW_OF_SCREEN_FULL    ; DE = A
    ld d,0
    add hl,de
    ld (TileMapAddressIndex),hl

    pop af ; Obtener el contador
    sub 1
    jp nz,CopyBlocksLoop
    ret

CopyBlocks2:
    call SetBlocksAlreadyCopied
    call UnsetCopyBlocks
    ;ld hl,(TilesColumn)
    ;ld (TilesColumnAddr),hl
    call InitTileMapIndex
    ;Aquí vamos a indicar en el mapa de tiles que se muestran por pantalla, la nueva columna a rellenar 
    ld hl,(TileMapAddressIndex) ; Indice de donde empieza la dirección de la tabla de tiles que se muestra en pantalla
    ld a,(IndexScrollScreen) ; Indice de por donde va el scroll real (columna invisible)
    ld e,a
    ld d,0
    add hl,de   ; Añade offset del scroll real para que copie en la posición correcta en vram los bloques del mapa completo en el siguiente paso
    ld (TileMapAddressIndex),hl
    ld a,SCREEN_HEIGHT_TILES    ; Contador de lineas vertical
CopyBlocksLoop2:
    push af ; Salvar el contador

    ; Copiamos el tile que corresponde del mapa completo
    ld hl,(TileMapIndex)
    ld de,(PointerBgScroll) ; Añade el offset del puntero hacia el mapa completo
    add hl,de
    ld de,(TilesColumn)
    ldi
    ldi

    ; Pasamos a apuntar a la siguiente fila del mapa completo
    ld hl,(TileMapIndex)
    ld bc,BG_WIDTH
    add hl,bc
    ld (TileMapIndex),hl

    pop af ; Obtener el contador
    sub 1
    jp nz,CopyBlocksLoop2

    ;ld hl,(TileMapAddressIndex)
    ;PrepareVram
    ;ld hl,()
    ;ld b,SCREEN_HEIGHT_TILES
    ;ld c, $be            ; Output port

    ret