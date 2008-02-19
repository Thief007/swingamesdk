@echo off

echo __________________________________________________
echo Creating Pascal Library for SwinGame
echo __________________________________________________

set BaseDir=%CD%
set LibOutput=.\bin\Win
set WinFPCDir=%BaseDir%\..\SDKs\Pascal\Win\FPC\lib\
set LibDir=%BaseDir%\lib\Win
set EXTRA_OPTS="-g -Sewn -vwn"

echo Running script from %BaseDir%
echo Saving output to %LibOutput%
echo Copying to FPC Win at %WinFPCDir%
echo Getting libraries from %LibDir%
echo Compiling with %EXTRA_OPTS%
echo __________________________________________________


if "%1"=="clean" goto cleaning

	if exist out.log del /q out.log
	if not exist "%LibOutput%" mkdir "%LibOutput%" >> out.log

	echo   Compiling Core
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Core.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Shapes
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Shapes.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Graphics
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Graphics.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Font
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Font.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Input
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Input.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Physics
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Physics.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Audio
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Audio.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling KeyCodes
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_KeyCodes.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling MappyLoader
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_MappyLoader.pas >> out.log
	if ERRORLEVEL 1 goto error
	echo   Compiling Camera
	fpc %EXTRA_OPTS% -Mdelphi -FE"%LibOutput%" -Fu"%LibOutput%" .\src\SGSDK_Camera.pas >> out.log
	if ERRORLEVEL 1 goto error
	
	echo   Copying to FPC
	if not exist "%WinFPCDir%" mkdir "%WinFPCDir%" >> out.log
	if ERRORLEVEL 1 goto error
	
	copy "%LibOutput%\*.ppu" "%WinFPCDir%" >> out.log
	if ERRORLEVEL 1 goto error
	copy "%LibOutput%\*.o" "%WinFPCDir%" >> out.log
	if ERRORLEVEL 1 goto error
	copy "%LibDir%\*.dll" "%WinFPCDir%" >> out.log
	if ERRORLEVEL 1 goto error

	echo   Finished
	echo __________________________________________________

goto end

:cleaning
	del /Q "%LibOutput%\SGSDK*.*"
	del /Q "%LibOutput%\sdl*.*"
	del /Q "%LibOutput%\libimp*.*"
	del /Q "%LibOutput%\smpeg.*"
	echo Cleaned
goto end

:error
	echo An error occurred
	type out.log

:end 