@echo off

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output=.\bin\Win
echo Saving output to %Output%

set WinFPCDir=%BaseDir%\..\SDKs\Pascal\Win\FPC\lib\
echo Copying to FPC Win at %WinFPCDir%

set LibDir=%BaseDir%\lib\Win
echo Getting libraries from %LibDir%

echo Doing %1

if "%1"=="clean" goto cleaning

REM - else build
	mkdir "%Output%"

	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Core.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Graphics.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Font.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Input.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Physics.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Audio.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_KeyCodes.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_MappyLoader.pas
	fpc -v0 -g -Mdelphi -FE"%Output%" .\src\SGSDK_Camera.pas

	echo Copying to FPC
	mkdir "%WinFPCDir%"
	copy "%Output%\*.ppu" "%WinFPCDir%"
	copy "%Output%\*.o" "%WinFPCDir%"
	copy "%LibDir%\*.dll" "%WinFPCDir%"

	echo Finished
goto end

:cleaning
	del /Q "%Output%\SGSDK*.*"
	del /Q "%Output%\sdl*.*"
	del /Q "%Output%\libimp*.*"
	del /Q "%Output%\smpeg.*"
	echo Cleaned
:end 