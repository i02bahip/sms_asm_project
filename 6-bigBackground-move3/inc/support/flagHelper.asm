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
    ld (ScrollStatus),a
    jp ContinueScrollStatus

ScrollDirectionRight:
    ld a,(ScrollStatus)
    or a,%00010101
    and a,%11111011
    ld (ScrollStatus),a
    jp ContinueScrollStatus

SetScrollZero:
    ld a,(ScrollStatus)
    and a,%11110111
    ld (ScrollStatus),a
    ret

NoScrollZero:
    ld a,(ScrollStatus)
    or a,%00001000
    ld (ScrollStatus),a
    ret

SetReachLeft:
    ld a,(ScrollStatus)
    and a,%11101111
    ld (ScrollStatus),a
    ret

UnsetReachLeft:
    ld a,(ScrollStatus)
    or a,%00010000
    ld (ScrollStatus),a
    ret

SetReachRight:
    ld a,(ScrollStatus)
    and a,%11011111
    ld (ScrollStatus),a
    ret

UnsetReachRight:
    ld a,(ScrollStatus)
    or a,%00100000
    ld (ScrollStatus),a
    ret

;------------------------------------------------
;------------------------------------------------
;   ACTION STATUS
    ; Bit 7: -
    ; Bit 6: -
    ; Bit 5: -
    ; Bit 4: Cambio de dirección
    ; Bit 3: Copiar bloques
    ; Bit 2: - Ultima direccion derecha
    ; Bit 1: -
    ; Bit 0: - Ultima direccion izquierda
;------------------------------------------------

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