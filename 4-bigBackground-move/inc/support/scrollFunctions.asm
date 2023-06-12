SetScrollStatus:
    call ScrollNoMovement
    ld a,(Controller)
    bit PLAYER1_JOYSTICK_LEFT,a
    call z, ScrollDirectionLeft
    bit PLAYER1_JOYSTICK_RIGHT,a
    call z, ScrollDirectionRight
    ret

ScrollNoMovement:
    ld a,(ScrollStatus)
    or a,%00000101
    ld (ScrollStatus),a
    ret

ScrollDirectionLeft:
    ld a,(ScrollStatus)
    and a,%11111110
    ld (ScrollStatus),a
    ret

ScrollDirectionRight:
    ld a,(ScrollStatus)
    and a,%11111011
    ld (ScrollStatus),a
    ret

MoveScroll:
    ld a,(ScrollStatus)
    bit 0,a
    jp z, AddScroll
    bit 2,a
    jp z, SubScroll
    
    ; Si no se han presionado botones, no hace falta checkear final de pantalla
    ld a,(Scroll)
    jp NoResetIndexScrollScreen

CheckEndScreen:
    jp nz,NoResetIndexScrollScreen
    ;Si entra aquí, se indica si ya se ha pintado la pantalla entera. 
    ;La variable Scroll ha llegado a 0 (va decrementandose de ff a 0). Ponemos el indice del tile a pintar al principio.
    ld (IndexScrollScreen),a
    call SetZeroScroll
NoResetIndexScrollScreen:
    ld (Scroll),a
    ret

SetZeroScroll:
    push af
    ld a,(ScrollStatus)
    and a,%11110111
    ld (ScrollStatus),a
    pop af
    ret

NoZeroScroll:
    push af
    ld a,(ScrollStatus)
    or a,%00001000
    ld (ScrollStatus),a
    pop af
    ret

SubScroll:
    ld a,(Scroll)
    sub SCROLL_HORIZONTAL_SPEED
    jp CheckEndScreen

AddScroll:
    ld a,(Scroll)
    add SCROLL_HORIZONTAL_SPEED
    jp CheckEndScreen

UpdateScrollIndexes:
    ld a,(ScrollStatus)
    bit 0,a
    jp z, SubScrolls
    bit 2,a
    call z, AddScrolls
ContinueUpdating:
    ret

AddScrolls:
    ld a,(IndexScrollScreen)
    add 2
    ld (IndexScrollScreen),a
    ld ix,(IndexBgScroll)
    inc ix
    inc ix
    ld (IndexBgScroll),ix
    ret

SubScrolls:
    ld a,(IndexScrollScreen)
    sub 2
    ld (IndexScrollScreen),a
    ld ix,(IndexBgScroll)
    dec ix
    dec ix
    ld (IndexBgScroll),ix
    jp ContinueUpdating