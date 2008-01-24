@echo off

echo Please run with Visual Studio Command Prompt

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output1=.\SGSDK.NET\lib\
echo Saving output to %Output1%

set LibDir=%BaseDir%\lib\Win
echo Getting libraries from %LibDir%

set DOTNETsln=.\SGSDK.NET\src\

set DOTNETbin=.\SGSDK.NET\src\bin\Debug\


set ShowcaseDOTNET=..\Showcase\CSharpDotNET\Showcase\lib\
if not exist "%ShowcaseDOTNET%" mkdir "%ShowcaseDOTNET%"
Showcase\CSharpDotNET\Showcase
set TOMATO=..\Demos\C#\TomatoQuest\lib\
if not exist %TOMATO% mkdir %TOMATO%

set SDK=..\SDKs\DOTNet\Visual Studio\C#\lib\
if not exist "%SDK%" mkdir "%SDK%"

set SDKVB=..\SDKs\DOTNet\Visual Studio\VB\lib\
if not exist "%SDKVB%" mkdir "%SDKVB%"

set DOTNETTest=..\Tests\Full\C#\lib\
if not exist %DOTNETTest% mkdir %DOTNETTest%

echo Doing %1

if "%1"=="clean" goto cleaning

	fpc -v0 -Mdelphi -FE"%Output1%" .\src\SGSDK.pas
	
	copy "%LibDir%\*.dll" "%Output1%"

	del /Q "%Output1%\*.o"
	del /Q "%Output1%\*.ppu"
	del /Q "%Output1%\*.a"

	msbuild "SGSDK.NET\src\SGSDK.NET.sln"

	copy "%DOTNETbin%\*.dll" "%Output1%"
	copy "%DOTNETbin%\*.xml" "%Output1%"

	echo Copying to Showcase
	copy "%Output1%\*.dll" "%ShowcaseDOTNET%"
	copy "%Output1%\*.xml" "%ShowcaseDOTNET%"

	echo Copying to TomatoQuest
	copy "%Output1%\*.dll" "%TOMATO%"
	copy "%Output1%\*.xml" "%TOMATO%"

	echo Copying to C# SDK
	copy "%Output1%\*.dll" "%SDK%"
	copy "%Output1%\*.xml" "%SDK%"

	echo Copying to VB SDK
	copy "%Output1%\*.dll" "%SDKVB%"
	copy "%Output1%\*.xml" "%SDKVB%"

	echo Copying to .NET Tests
	copy "%Output1%\*.dll" "%DOTNETTest%"
	copy "%Output1%\*.xml" "%DOTNETTest%"

	echo Finished
goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
:end 