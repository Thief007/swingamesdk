@echo off

echo __________________________________________________
echo Copying Visual Studio Files
echo __________________________________________________

if exist out.log del /q out.log

set VSCS=.\Visual Studio\C#
set VSVB=.\Visual Studio\VB
set CLCS=.\Command Line\C#
set CLVB=.\Command Line\VB

set FromCS=..\Base\DOTNet\C#
set FromVB=..\Base\DOTNet\VB

set ExtraCS=Properties
set ExtraVB=My Project

set SDK=%VSCS%
set CodeFrom=%FromCS%
set ExtraFolder=%ExtraCS%

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

echo   Copying Code to %SDK%
copy "%CodeFrom%\*.*" "%SDK%" >> out.log
if ERRORLEVEL 1 goto error

if not exist "%SDK%\%ExtraFolder%" mkdir "%SDK%\%ExtraFolder%"
if ERRORLEVEL 1 goto error

copy "%CodeFrom%\%ExtraFolder%\*" "%SDK%\%ExtraFolder%" >> out.log
if ERRORLEVEL 1 goto error

echo   Finished %SDK%
echo ....

if "%SDK%" == "%VSCS%" goto CPCLCS
if "%SDK%" == "%CLCS%" goto CPVSVB
if "%SDK%" == "%VSVB%" goto CPCLVB
goto end

:CPCLCS
	set SDK=%CLCS%
	set CodeFrom=%FromCS%
	set ExtraFolder=%ExtraCS%
	goto docopy

:CPVSVB
	set SDK=%VSVB%
	set CodeFrom=%FromVB%
	set ExtraFolder=%ExtraVB%
	goto docopy

:CPCLVB
	set SDK=%CLVB%
	set CodeFrom=%FromVB%
	set ExtraFolder=%ExtraVB%
	goto docopy


goto end

:error
	echo An error occurred log in out.log

:end 
echo __________________________________________________
