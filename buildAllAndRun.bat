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

call generateMap.bat %1
call build.bat %1
call run.bat %1
if errorlevel 1 goto :fail
goto :done

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
goto:eof