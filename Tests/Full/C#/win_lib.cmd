@echo off

echo __________________________________________________
echo Copying Library Files and Resources
echo __________________________________________________

if exist out.log del /q out.log

if exist .\lib goto RemoveLib
goto MakeLib

:RemoveLib
 echo   Removing old lib dir
 del /q .\lib  >> out.log
 rmdir lib >> out.log

:MakeLib
echo   Making new lib folder
mkdir lib >> out.log

SET LIB_PATH="..\..\..\SDKs\DOTNet\Visual Studio\C#"

echo   Copying lib from %LIB_PATH%
copy  %LIB_PATH%\lib\* .\lib >> out.log

echo   Copying Resources

SET SOURCE_RESOURCE=..\Pascal\Resources
SET RESOURCE_DIR=.\Resources

if not exist %RESOURCE_DIR% goto MakeResourceDIR
goto CopyResources

:MakeResourceDIR
	mkdir %RESOURCE_DIR% >> out.log
	mkdir %RESOURCE_DIR%\fonts >> out.log
	mkdir %RESOURCE_DIR%\images >> out.log
	mkdir %RESOURCE_DIR%\sounds >> out.log
	mkdir %RESOURCE_DIR%\maps >> out.log

:CopyResources

copy /Y %SOURCE_RESOURCE%\*.* %RESOURCE_DIR% >> out.log
copy /Y %SOURCE_RESOURCE%\fonts\* %RESOURCE_DIR%\fonts >> out.log
copy /Y %SOURCE_RESOURCE%\images\* %RESOURCE_DIR%\images >> out.log
copy /Y %SOURCE_RESOURCE%\sounds\* %RESOURCE_DIR%\sounds >> out.log
copy /Y %SOURCE_RESOURCE%\maps\* %RESOURCE_DIR%\maps >> out.log

echo   Finished
echo __________________________________________________
