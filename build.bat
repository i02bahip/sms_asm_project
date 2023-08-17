@echo off
cls
IF "%1"=="0" goto No0
IF "%1"=="1" goto No1 
IF "%1"=="2" goto No2
IF "%1"=="3" goto No3
IF "%1"=="4" goto No4
IF "%1"=="5" goto No5
IF "%1"=="6" goto No6
IF "%1"=="7" goto No7
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
:No5
    set folder="5-bigBackground-move2"
    goto EndParams
:No6
    set folder="6-bigBackground-move3"
    goto EndParams
:No7
    set folder="7-bigBackground-move4"
    goto EndParams
:EndParams


cd %folder%
..\environment\compiler\wla-dx\bin\x86\wla-z80 -v -o ..\out\main.o example-%1.asm
if errorlevel 1 goto :fail
..\environment\compiler\wla-dx\bin\x86\wlalink -d -v -s ..\environment\compiler\wla-dx\link\link.lk ..\out\example-%1.sms
if errorlevel 1 goto :fail
goto :done

:fail
echo Build failed!
cd ..
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
echo 5: big background move2
echo 6: big background move3
echo 7: big background move4
echo ----------------------------
echo Example for build example 0: 
echo build 0
goto:endProcess

:endProcess
goto:eof

:done
rem cleanup
if exist ..\out\*.o del ..\out\*.o
if exist ..\out\.wla* del ..\out\.wla*
echo Build finished.
cd ..