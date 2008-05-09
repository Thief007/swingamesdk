@echo off

echo __________________________________________________
echo Creating .NET Library for SwinGame
echo __________________________________________________

echo Please run with Visual Studio Command Prompt

set BaseDir=%CD%
set Output1=.\SGSDK.NET\lib\
set LibDir=%BaseDir%\lib\Win

set DOTNETsln=.\SGSDK.NET\src\
set DOTNETbin=.\SGSDK.NET\src\bin\Debug\

set SDKVB=..\SDKs\DOTNet\Visual Studio\VB\lib\
set SDK=..\SDKs\DOTNet\Visual Studio\C#\lib\

set CLSDKVB=..\SDKs\DOTNet\Command Line\VB\lib\
set CLSDK=..\SDKs\DOTNet\Command Line\C#\lib\

set EXTRA_OPTS="-O3 -Sewn -vwn -wr"

echo Running script from %BaseDir%
echo Saving output to %Output1%
echo Getting libraries from %LibDir%
echo Saving C# SDK to %SDK%
echo Saving VB.NET SDK to %SDKVB%
echo Compiling SGSDK with %EXTRA_OPTS%
echo __________________________________________________

if not exist "%SDKVB%" mkdir "%SDKVB%"
if not exist "%SDK%" mkdir "%SDK%"
if not exist "%CLSDKVB%" mkdir "%CLSDKVB%"
if not exist "%CLSDK%" mkdir "%CLSDK%"

if exist out.log del /q out.log

if "%1"=="clean" goto cleaning
	echo   Compiling SGSDK.dll
	fpc -v0 -Mdelphi -FE"%Output1%" .\src\SGSDK.pas >> out.log
	if ERRORLEVEL 1 goto error
	
	echo   Copying library to output
	copy "%LibDir%\*.dll" "%Output1%" >> out.log
	if ERRORLEVEL 1 goto error
	
	echo   Cleaning up library compile
	del /Q "%Output1%\*.o" >> out.log
	del /Q "%Output1%\*.ppu" >> out.log

	echo   Compiling SGSDK.NET.dll
	msbuild "SGSDK.NET\src\SGSDK.NET.sln" >> out.log

	echo   Copying dll to output
	copy "%DOTNETbin%\*.dll" "%Output1%" >> out.log
	copy "%DOTNETbin%\*.xml" "%Output1%" >> out.log

	echo   Copying to VS C# SDK
	copy "%Output1%\*.dll" "%SDK%" >> out.log
	copy "%Output1%\*.xml" "%SDK%" >> out.log

	echo   Copying to VS VB SDK
	copy "%Output1%\*.dll" "%SDKVB%" >> out.log
	copy "%Output1%\*.xml" "%SDKVB%" >> out.log

	echo   Copying to CL C# SDK
	copy "%Output1%\*.dll" "%CLSDK%" >> out.log
	copy "%Output1%\*.xml" "%CLSDK%" >> out.log

	echo   Copying to CL VB SDK
	copy "%Output1%\*.dll" "%CLSDKVB%" >> out.log
	copy "%Output1%\*.xml" "%CLSDKVB%" >> out.log

	echo   Finished
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
