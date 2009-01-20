@echo off

set VERSION=2.0
set VERSION_TXT=2-0

echo __________________________________________________
echo  Creating WIN SDKs for - %VERSION%
echo __________________________________________________

set OPT=1

:THE_START
	if %OPT% == 1 goto FPC
	if %OPT% == 2 goto DELPHI
	if %OPT% == 3 goto CSHARP
	if %OPT% == 4 goto VBDotNET
	if %OPT% == 5 goto VB6
	
	goto THE_END
	
:FPC
	set NAME=FPC
	set COPY_SDK_DIR=.\Pascal\Win\FPC
	set BACK_DIR=..\..\..\
	goto PACK

:DELPHI
	set NAME=Delphi
	set COPY_SDK_DIR=.\Pascal\Win\Delphi
	set BACK_DIR=..\..\..\
	goto PACK
	
:CSHARP
	set NAME=C#
	set COPY_SDK_DIR=.\DOTNet\Command Line\C#
	set BACK_DIR=..\..\..\
	goto PACK
	
:VBDotNET
	set NAME=VB.NET
	set COPY_SDK_DIR=.\DOTNet\Command Line\VB
	set BACK_DIR=..\..\..\
	goto PACK
	
:VB6
	set NAME=VB6
	set COPY_SDK_DIR=.\VB6\Visual Studio 6.0
	set BACK_DIR=..\..\
	goto PACK
	
REM
REM Pack folder %COPY_SDK_DIR% to distribute
REM With name $2
REM
:Pack
	set BASE_SDK_DIR="SGSDK-%VERSION_TXT%-%NAME%-WIN"
	set SDK_DIR=.\SwinGame
	
	echo  Creating %BASE_SDK_DIR%
	
	if exist "%BASE_SDK_DIR%.zip" goto DEL_OLD:
	goto AFTER_DEL_OLD
	
	:DEL_OLD
		echo  Removing old SDK - %BASE_SDK_DIR%.zip
		del "%BASE_SDK_DIR%.zip"
	
	:AFTER_DEL_OLD
	
	if exist "%BASE_SDK_DIR%" goto DEL_OLD_SDK
	goto AFTER_DEL_SDK
	
	:DEL_OLD_SDK
		del /s /q "%BASE_SDK_DIR%"	
		rmdir /s /q "%BASE_SDK_DIR%"	
		
	:AFTER_DEL_SDK
	
	pushd .
	
	mkdir "%BASE_SDK_DIR%"
	cd "%BASE_SDK_DIR%"
	mkdir "%SDK_DIR%"
	cd "%SDK_DIR%"

	cd ..\..\
	cd "%COPY_SDK_DIR%"
	
	echo  Copying SDK to temporary location
	xcopy . %BACK_DIR%\%BASE_SDK_DIR%\SwinGame /E /Q /Y /EXCLUDE:%BACK_DIR%\ExcludeList.txt

	echo  Combining files and zipping
	popd
	zip -q -r -m -9 "%BASE_SDK_DIR%.zip" "%BASE_SDK_DIR%"
	
	echo  Done %BASE_SDK_DIR%
	echo  --

if %OPT% == 5 set OPT=6
if %OPT% == 4 set OPT=5
if %OPT% == 3 set OPT=4
if %OPT% == 2 set OPT=3
if %OPT% == 1 set OPT=2

goto THE_START

:THE_END