@echo off

set PRODUCT_NAME=%1
set ICON=%2

if Not Defined PRODUCT_NAME goto usage
if Not Defined ICON set ICON=SwinGame.ico

echo %ICON%

echo ************************
echo Compiling game using FPC
echo ************************
echo Output can be found in the bin folder

if exist GameLauncher.rc del GameLauncher.rc
echo grIcon ICON "%ICON%" >> GameLauncher.rc
cp .\Resources\%ICON% .
if errorlevel 1 goto error

windres GameLauncher.rc GameLauncher.res
if errorlevel 1 goto error

fpc -Mdelphi -FE.\bin -Fu.\lib\Pas -o"%PRODUCT_NAME%" GameLauncher.pas
if errorlevel 1 goto error

del /q .\bin\*.o .\bin\*.ppu
del GameLauncher.rc
del GameLauncher.res
del GameLauncher.or
del %ICON%

echo ... done
echo ************************
echo Copying resources to bin directory
echo ************************

CopyResources.cmd

echo ... done
echo ************************
echo Copying libraries to bin
echo ************************

cp -u .\lib\*.dll .\bin
echo ... done
goto end

:usage 
	echo Usage: %0 productName [iconfile]

:error
	echo Error...

:end