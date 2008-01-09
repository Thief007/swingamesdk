@echo off

echo Please run with Visual Studio Command Prompt
echo Please run DotNet_lib.cmd first

pause

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output1=.\SGSDKVB6\lib\
echo Saving output to %Output1%

set LibDir=%BaseDir%\SGSDK.NET\lib
echo Getting libraries from %LibDir%

set VB6bin=.\SGSDKVB6\src\bin\Debug\

set ShowcaseVB6=..\Showcase\VB6\lib\

echo Doing %1

if "%1"=="clean" goto cleaning
	copy "%LibDir%\*.dll" "%Output1%"
	copy "%LibDir%\*.xml" "%Output1%"

	msbuild "SGSDKVB6\src\SGSDKVB6.sln"

	copy "%VB6bin%*.dll" "%Output1%"
	copy "%VB6bin%*.xml" "%Output1%"

	RegAsm %Output1%SGSDKVB6.dll /unregister
	RegAsm %Output1%SGSDKVB6.dll /tlb:%Output1%com.SGSDKVB6.tlb
	RegAsm %Output1%SGSDKVB6.dll /regfile:%Output1%SGSDKVB6.reg

	echo Copying to Showcase
	copy "%Output1%*" "%ShowcaseVB6%"

	echo Finished
goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
:end 