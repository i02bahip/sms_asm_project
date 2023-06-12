InitTileMapIndex:
    ld hl,TileMap
    ld (TileMapIndex),hl
    ld hl,TILEMAP_ADDRESS
    ld (TileMapAddressIndex),hl
    ret

InitTileMapIndexInVisibleColumns:
    ld hl,TileMap
    ld (TileMapIndex),hl
    ld hl,TILEMAP_ADDRESS_INIT
    ld (TileMapAddressIndex),hl
    ret