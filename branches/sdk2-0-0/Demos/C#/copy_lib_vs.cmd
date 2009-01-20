@echo off

if exist ".\%1%" goto run  

echo Usage: %0% [demo]
goto end

:run

set DEMO_NAME="%1%"

cd .\%DEMO_NAME%

echo __________________________________________________
echo Copying .NET Files
echo __________________________________________________

if not exist .\lib goto pt1  

	echo    Removing old lib dir
	del /q /f .\lib
	rmdir .\lib
:pt1

echo    Making new lib folder
mkdir .\lib

set LIB_PATH="..\..\..\SDKs\DOTNet\Visual~1\C#\lib\*"

echo    Copying lib from %LIB_PATH%
copy "%LIB_PATH%"  .\lib >> out.log

del /q out.log

echo   Finished

cd ..

:end
