GetCtrl:
    push af
    in a,$dc
    ld [Controller],a
    pop af
    ret
ButtonHandle:
    push af
    ld a,[Controller]
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