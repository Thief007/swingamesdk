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


set ShowcaseDOTNET=..\Showcase\CSharpDotNET\SGSDK Showcase\lib\
set RPGDEMO=..\Demos\C#\RPGDemo\lib\

set SDK=..\SDKs\DOTNet\Visual Studio\C#\lib\
set SDKVB=..\SDKs\DOTNet\Visual Studio\VB\lib\

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

	echo Copying to RPGDemo
	copy "%Output1%\*.dll" "%RPGDEMO%"
	copy "%Output1%\*.xml" "%RPGDEMO%"

	echo Copying to C# SDK
	copy "%Output1%\*.dll" "%SDK%"
	copy "%Output1%\*.xml" "%SDK%"

	echo Copying to VB SDK
	copy "%Output1%\*.dll" "%SDKVB%"
	copy "%Output1%\*.xml" "%SDKVB%"

	echo Finished
goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
:end 