@echo off

echo __________________________________________________
echo Copying Visual Studio Files
echo __________________________________________________

if exist out.log del /q out.log

set VSVB=.\Visual Studio 6.0\

set SDK=%VSVB%

:docopy

echo   Copying files to %SDK%
copy ..\Base\All\* "%SDK%" >> out.log
if ERRORLEVEL 1 goto error

if exist "%SDK%\Resources" goto AfterMkDir

echo   Creating Folder %SDK%\Resources
mkdir "%SDK%\Resources" >> out.log
if ERRORLEVEL 1 goto error

mkdir "%SDK%\Resources\fonts" >> out.log
if ERRORLEVEL 1 goto error

mkdir "%SDK%\Resources\images" >> out.log
if ERRORLEVEL 1 goto error

mkdir "%SDK%\Resources\sounds" >> out.log
if ERRORLEVEL 1 goto error

mkdir "%SDK%\Resources\maps" >> out.log
if ERRORLEVEL 1 goto error

:AfterMkDir

echo   Copying Resources to %SDK%\Resources
copy ..\Base\All\Resources\* "%SDK%\Resources" >> out.log
if ERRORLEVEL 1 goto error
copy ..\Base\All\Resources\fonts\* "%SDK%\Resources\fonts" >> out.log
if ERRORLEVEL 1 goto error
copy ..\Base\All\Resources\images\* "%SDK%\Resources\images" >> out.log
if ERRORLEVEL 1 goto error
copy ..\Base\All\Resources\sounds\* "%SDK%\Resources\sounds" >> out.log
if ERRORLEVEL 1 goto error
REM copy ..\Base\All\Resources\maps\* "%SDK%\Resources\maps" >> out.log
REM if ERRORLEVEL 1 goto error

echo   Finished %SDK%
echo ....

goto end

:error
	echo An error occurred log in out.log

:end 
echo __________________________________________________
