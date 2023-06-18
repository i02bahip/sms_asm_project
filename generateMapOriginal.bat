@echo off
cls
IF "%1"=="0" goto No0
IF "%1"=="1" goto No1 
IF "%1"=="2" goto No2
IF "%1"=="3" goto No3
IF "%1"=="4" goto No4
goto :showParams

:No0
    set folder="0-completeExample"
    goto EndParams
:No1
    set folder="1-helloWorld"
    goto EndParams
:No2
    set folder="2-background"
    goto EndParams
:No3
    set folder="3-bigBackground"
    goto EndParams
:No4
    set folder="4-bigBackground-move"
    goto EndParams
:EndParams

set BMP2TILE="environment\utils\bmp2tile\BMP2Tile.exe"
if exist %folder%\inc\art\tileMaps\bg*.inc del %folder%\inc\art\tileMaps\bg*.inc
if exist %folder%\inc\art\palettes\bg*.inc del %folder%\inc\art\palettes\bg*.inc
if exist %folder%\inc\art\tileSets\bg*.inc del %folder%\inc\art\tileSets\bg*.inc

%BMP2TILE% "%folder%\inc\art\backgroundImage\tilemap_limits.png" -savetiles "%folder%\inc\art\tileSets\bgTileSet.inc" -savepalette "%folder%\inc\art\palettes\bgPalette.inc" -savetilemap "%folder%\inc\art\tileMaps\bgTileMap.inc" -exit
if errorlevel 1 goto :fail
goto:done

:endProcess
goto:eof

:fail
echo Map generation failed!
goto:endProcess

:showParams
echo Wrong param: "%1"
echo The correct format is: build param
echo Possible params:
echo ----------------------------
echo 0: complete example
echo 1: hello world
echo 2: background
echo 3: big background
echo 4: big background move
echo ----------------------------
echo Example for build example 0: 
echo build 0
goto:endProcess

:done
echo Palette, tileset and tilemap generated successfully!