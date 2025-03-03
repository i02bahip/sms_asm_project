;------------------------------------------------
;------------------------------------------------
;   SCROLL STATUS
    ; Bit 7: Limite inferior
    ; Bit 6: Limite superior
    ; Bit 5: Límite derecho
    ; Bit 4: Límite izquierdo
    ; Bit 3: scroll zero
    ; Bit 2: Dirección derecha
    ; Bit 1: Dirección arriba 
    ; Bit 0: Dirección izquierda
;------------------------------------------------

ScrollNoMovement:
    ld a,(ScrollStatus)
    or a,%00000101
    ld (ScrollStatus),a
    ret 

ScrollDirectionLeft:
    ld a,(ScrollStatus)
    or a,%00100101
    and a,%11111110
    ld (ScrollStatus), a
    call UnsetBlocksAlreadyCopied
    ld a,(ActionStatus)
    bit 2,a
    call z,SetDirChanged
    ld a,(ActionStatus)
    bit 2,a
    call nz,UnsetDirChanged
    ret

ScrollDirectionRight:
    ld a,(ScrollStatus)
    or a,%00010101
    and a,%11111011
    ld (ScrollStatus),a
    call UnsetBlocksAlreadyCopied
    ld a,(ActionStatus)
    bit 0,a
    call z,SetDirChanged
    ld a,(ActionStatus)
    bit 0,a
    call nz,UnsetDirChanged
    ret

;------------------------------------------------
;------------------------------------------------
;   ACTION STATUS
    ; Bit 7: -
    ; Bit 6: -
    ; Bit 5: -
    ; Bit 4: Bloques ya copiados
    ; Bit 3: Copiar bloques
    ; Bit 2: - Ultima direccion derecha
    ; Bit 1: - Cambio de direccion
    ; Bit 0: - Ultima direccion izquierda
;------------------------------------------------

;----------- NO USADOS PERO PUEDEN SER ÚTILES----

SetCopyBlocks:
    ld a,(ActionStatus)
    or a,%00001000
    ld (ActionStatus),a
    ret

UnsetCopyBlocks:
    ld a,(ActionStatus)
    and a,%11110111
    ld (ActionStatus),a
    ret

UnsetBlocksAlreadyCopied:
    ld a,(ActionStatus)
    or a,%00010000
    ld (ActionStatus),a
    ret

SetBlocksAlreadyCopied:
    ld a,(ActionStatus)
    and a,%11101111
    ld (ActionStatus),a
    ret

;------------------------------------------------

LastDirectionLeft:
    ld a,(ActionStatus)
    and a,%11111110
    or a,%00000100
    ld (ActionStatus),a
    ret

LastDirectionRight:
    ld a,(ActionStatus)
    and a,%11111011
    or a,%00000001
    ld (ActionStatus),a
    ret

UnsetDirChanged:
    ld a,(ActionStatus)
    or a,%00000010
    ld (ActionStatus),a
    ret

SetDirChanged:
    ld a,(ActionStatus)
    and a,%11111101
    ld (ActionStatus),a
    ret

ResetBGColor:
    xor a
    ld (BgColor),a
    ret

ChangeBGColor:
    ld a,(BgColor)
    add 1
    ld (BgColor),a
    ret