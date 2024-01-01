CopyBlocks:
    call SetBlocksAlreadyCopied
    call UnsetCopyBlocks
    call InitVRAMAndMemoryIndex
    ;Aquí vamos a indicar en el mapa de tiles que se muestran por pantalla, la nueva columna a rellenar 
    ld hl,(MapVRAMIndex) ; Indice de donde empieza la dirección de la tabla de tiles que se muestra en pantalla
    ld a,(RealScrollScreen) ; Indice de por donde va el scroll real (columna invisible)
    ld e,a
    ld d,0
    add hl,de   ; Añade offset del scroll real para que copie en la posición correcta en vram los bloques del mapa completo en el siguiente paso
    ld (MapVRAMIndex),hl
    ld a,SCREEN_HEIGHT_TILES    ; Contador de lineas vertical
CopyBlocksLoop:
    push af ; Salvar el contador

    ;-----------------------------------------------------------------------------
    ; 1) Apuntamos a donde vamos a copiar en la VRAM
    ;-----------------------------------------------------------------------------
    ld hl,(MapVRAMIndex)
    PrepareVram

    ;-----------------------------------------------------------------------------
    ; 2) Apuntamos al tile del escenario completo en memoria para copiarlo en la VRAM
    ; hl -> direccion a copiar
    ; bc -> tamaño a copiar
    ;-----------------------------------------------------------------------------
    ld hl,(MapMemoryIndex)
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
    ld hl,(MapMemoryIndex)
    ld bc,BG_WIDTH
    add hl,bc
    ld (MapMemoryIndex),hl

    ;-----------------------------------------------------------------------------
    ; Apuntamos a la siguiente fila del mapa en la VRAM
    ;-----------------------------------------------------------------------------
    ld hl,(MapVRAMIndex)
    ld e,SCREEN_WIDTH    ; DE = A
    ld d,0
    add hl,de
    ld (MapVRAMIndex),hl

    pop af ; Obtener el contador
    sub 1
    jp nz,CopyBlocksLoop
    ret