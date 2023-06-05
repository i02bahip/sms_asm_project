;===========================================================================
;		BANK 03 - 
;			Unknown. 
;===========================================================================
.BANK 1
.ORG $0000
Bank1:
RacetrackMockupData:
   .dw RacetrackTiles    ; Pointer to tile data.
   .dw RacetrackTilesEnd-RacetrackTiles ; Tile data (bytes) to write.
   .dw RacetrackTilemap ; Pointer to tilemap data.
   .dw WHOLE_NAMETABLE  ; Overwrite the whole nametable.
   .dw RacetrackPalette ; Pointer to palette.
   .dw RacetrackPaletteEnd-RacetrackPalette ; Amount of colors.
RacetrackTiles:
   ;.include "Race/tiles.inc"
   .include "Race/rt_tiles.inc"
RacetrackTilesEnd:
RacetrackTilemap:
   ;.include "Race/tilemap.inc"
   .include "Race/rt_tilemap.inc"
RacetrackPalette:
   .include "Race/rt_palette.inc"
RacetrackPaletteEnd: