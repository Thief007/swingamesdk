@echo off

color f0

echo __________________________________________________
echo  ¦                                              ¦
echo  ¦ Please run with Visual Studio Command Prompt ¦
echo  ¦                                              ¦
echo  ¦       Please run DotNet_lib.cmd first        ¦
echo  ¦                                              ¦
echo  ¦                                              ¦
echo  ¦            VB6 Must not be open!!            ¦
echo  ¦                                              ¦
echo __________________________________________________

pause
color

if exist out.log del /q out.log

set BaseDir=%CD%
set Output1=.\SGSDKVB6\lib\
set LibDir=%BaseDir%\SGSDK.NET\lib

set SDKVB6=..\SDKs\VB6\Visual Studio 6.0\lib\
if not exist "%SDKVB6%" mkdir "%SDKVB6%"

set VB6bin=.\SGSDKVB6\src\bin\Debug\


echo Running script from %BaseDir%
echo Saving output to %Output1%
echo Getting libraries from %LibDir%
if not exist %LibDir% goto error

echo VB6 SDK set to %SDKVB6%
echo VB6 bin set to %VB6bin%
echo __________________________________________________


REM set ShowcaseVB6=..\Showcase\VB6\lib\
REM if not exist %ShowcaseVB6% mkdir %ShowcaseVB6%

REM set airhocky=..\Demos\VB6\Air Hocky\lib\

if "%1"=="clean" goto cleaning

	echo   Coping Library to output
	copy "%LibDir%\*.dll" "%Output1%" >> out.log
	if ERRORLEVEL 1 goto error
	copy "%LibDir%\*.xml" "%Output1%" >> out.log
	if ERRORLEVEL 1 goto error

	echo   Compiling SwinGame COM interface
	msbuild "SGSDKVB6\src\SGSDKVB6.sln" >> out.log
	if ERRORLEVEL 1 goto error

	echo   Copying COM interface to output
	copy "%VB6bin%*.dll" "%Output1%" >> out.log
	if ERRORLEVEL 1 goto error
	copy "%VB6bin%*.xml" "%Output1%" >> out.log
	if ERRORLEVEL 1 goto error

	echo   Registering SGSDKVB6.dll
	RegAsm %Output1%SGSDKVB6.dll /unregister >> out.log
	if ERRORLEVEL 1 goto error
	RegAsm %Output1%SGSDKVB6.dll /tlb:%Output1%com.SGSDKVB6.tlb /codebase >> out.log
	if ERRORLEVEL 1 goto error
	RegAsm %Output1%SGSDKVB6.dll /regfile:%Output1%SGSDKVB6.reg >> out.log
	if ERRORLEVEL 1 goto error

	echo Copying to SDKVB6
	copy "%Output1%*" "%SDKVB6%" >> out.log
	if ERRORLEVEL 1 goto error

	echo Finished
	echo __________________________________________________

goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
goto end
	
:error
	echo An error occurred
	type out.log
:end 

