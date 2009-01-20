@echo off

set LIB_PATH==..\..\SDKs\Pascal\Win\FPC

echo "Copying lib from %LIB_PATH%"

if exist lib del lib
mkdir lib

copy %LIB_PATH%\lib\* .\lib\
copy %LIB_PATH%\*.cmd .\*.cmd

