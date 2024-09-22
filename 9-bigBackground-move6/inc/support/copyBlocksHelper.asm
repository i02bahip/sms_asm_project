CopyBlocks:
    ld a,SCREEN_HEIGHT_TILES    ; Contador de lineas vertical
CopyBlocksLoop:
    push af ; Salvar el contador

    ;-----------------------------------------------------------------------------
    ; 1) Apuntamos a donde vamos a copiar en la VRAM
    ;-----------------------------------------------------------------------------
    ld hl,(CopyMapVRAMIndex)
    PrepareVram

    ;-----------------------------------------------------------------------------
    ; 2) Apuntamos al tile del escenario completo en memoria para copiarlo en la VRAM
    ; hl -> direccion a copiar
    ; bc -> tamaño a copiar
    ;-----------------------------------------------------------------------------
    ld hl,(CopyMapMemoryIndex)
    ld de,(PointerBgScroll) ; Añade el offset del puntero hacia el mapa completo
    add hl,de
    ld bc,2  ; Counter for number of bytes to write

    ;-----------------------------------------------------------------------------
    ; 3) Se copia en la dirección de VRAM (punto 1) 
    ; la dirección/tamaño de memoria indicado en el punto 2
    ;-----------------------------------------------------------------------------
    call LoadVRAM

    ;-----------------------------------------------------------------------------
    ; Pasamos a apuntar a la siguiente fila del mapa completo
    ;-----------------------------------------------------------------------------
    ld hl,(CopyMapMemoryIndex)
    ld bc,BG_WIDTH
    add hl,bc
    ld (CopyMapMemoryIndex),hl

    ;-----------------------------------------------------------------------------
    ; Apuntamos a la siguiente fila del mapa en la VRAM
    ;-----------------------------------------------------------------------------
    ld hl,(CopyMapVRAMIndex)
    ld e,SCREEN_WIDTH    ; DE = A
    ld d,0
    add hl,de
    ld (CopyMapVRAMIndex),hl

    pop af ; Obtener el contador
    sub 1
    jp nz,CopyBlocksLoop
    ret