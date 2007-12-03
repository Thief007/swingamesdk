@echo off

echo Please run with Visual Studio Command Prompt

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output1=.\SGSDK.NET\lib\
echo Saving output to %Output%

set Output2=.\SGSDK.NET\lib\
echo Saving output to %Output2%

set LibDir=%BaseDir%\lib\Win
echo Getting libraries from %LibDir%

set DOTNETsln=.\SGSDK.NET\src\

set DOTNETbin=.\SGSDK.NET\src\bin\Debug\

echo Doing %1

if "%1"=="clean" goto cleaning

	fpc -v0 -Mdelphi -FE"%Output1%" .\src\SGSDK.pas
	
	copy "%LibDir%\*.dll" "%Output1%"

	del /Q "%Output1%\*.o"
	del /Q "%Output1%\*.ppu"
	del /Q "%Output1%\*.a"

	msbuild "SGSDK.NET\src\SGSDK.NET.sln"

	copy "%DOTNETbin%\*.dll" "%Output2%"
	copy "%DOTNETbin%\*.xml" "%Output2%"
	


	echo Finished
goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
:end 