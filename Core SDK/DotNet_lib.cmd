@echo off

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output=.\SGSDK.NET\lib\
echo Saving output to %Output%

set LibDir=%BaseDir%\lib\Win
echo Getting libraries from %LibDir%

echo Doing %1

if "%1"=="clean" goto cleaning

	fpc -v0 -Mdelphi -FE"%Output%" .\src\SGSDK.pas
	
	copy "%LibDir%\*.dll" "%Output%"

	del /Q "%Output%\*.o"
	del /Q "%Output%\*.ppu"
	del /Q "%Output%\*.a"

	echo Finished
goto end

:cleaning
	del /Q "%Output%\SGSDK*.*"
	del /Q "%Output%\sdl*.*"
	del /Q "%Output%\libimp*.*"
	del /Q "%Output%\smpeg.*"
	echo Cleaned
:end 