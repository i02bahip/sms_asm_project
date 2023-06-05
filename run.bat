@echo off
if [%1]==[] goto :showParams

echo Running: %folder%
java -jar environment\emulators\emulicious\Emulicious.jar out\example-%1.sms
goto:endProcess

:showParams
echo Wrong param: "%1"
echo The correct format is: run param
echo Possible params:
echo ----------------------------
echo 0: complete example
echo 1: hello world
echo 2: background
echo ----------------------------
echo Example for run example 0: 
echo run 0
goto:endProcess

:endProcess
goto:eof