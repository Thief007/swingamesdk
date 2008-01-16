@echo off
color f0
echo        +==============================================+
echo        ¦                                              ¦
echo        ¦ Please run with Visual Studio Command Prompt ¦
echo        ¦                                              ¦
echo        ¦       Please run DotNet_lib.cmd first        ¦
echo        ¦                                              ¦
echo        ¦                                              ¦
echo        ¦            VB6 Must not be open!!            ¦
echo        ¦                                              ¦
echo        +==============================================+

 
pause
color

set BaseDir=%CD%
echo Running script from %BaseDir%

set Output1=.\SGSDKVB6\lib\
echo Saving output to %Output1%

set LibDir=%BaseDir%\SGSDK.NET\lib
echo Getting libraries from %LibDir%
if not exist %LibDir% mkdir %LibDir%

set VB6bin=.\SGSDKVB6\src\bin\Debug\

set ShowcaseVB6=..\Showcase\VB6\lib\
if not exist %ShowcaseVB6% mkdir %ShowcaseVB6%

set SDKVB6=..\SDKs\VB6\Visual Studio 6.0\lib\
if not exist %SDKVB6% mkdir %SDKVB6%

set airhocky=..\Demos\VB6\Air Hocky\lib\

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

	echo Copying to air hocky
	copy "%Output1%*" "%airhocky%"

	echo Copying to SDKVB6
	echo %SDKVB6%
	copy "%Output1%*" "%SDKVB6%"

	echo Finished
goto end

:cleaning
	del /Q "%Output1%\SGSDK*.*"
	del /Q "%Output1%\sdl*.*"
	del /Q "%Output1%\libimp*.*"
	del /Q "%Output1%\smpeg.*"
	echo Cleaned
:end 