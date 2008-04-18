@echo off

echo __________________________________________________
echo Copying Mac/Linux Scripts
echo __________________________________________________


if not exist .\lib mkdir .\lib

set LIB_PATH=..\..\..\SDKs\Pascal\Win\FPC

xcopy %LIB_PATH%\lib\* .\lib\ /e /y /q
xcopy %LIB_PATH%\*.cmd . /e /y /q
