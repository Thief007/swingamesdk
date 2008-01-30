@echo off

echo __________________________________________________
echo Copying Visual Studio Files
echo __________________________________________________

copy ..\Base\All\* ".\Visual Studio\C#"

if exist ".\Visual Studio\C#\Resources" goto AfterMkDir

echo Creating Resources Directory
mkdir ".\Visual Studio\C#\Resources"
mkdir ".\Visual Studio\C#\Resources\fonts"
mkdir ".\Visual Studio\C#\Resources\images"
mkdir ".\Visual Studio\C#\Resources\sounds"
mkdir ".\Visual Studio\C#\Resources\maps"

:AfterMkDir
if exist ".\Visual Studio\VB\Resources" goto AfterMkDir2

mkdir ".\Visual Studio\VB\Resources"
mkdir ".\Visual Studio\VB\Resources\fonts"
mkdir ".\Visual Studio\VB\Resources\images"
mkdir ".\Visual Studio\VB\Resources\sounds"
mkdir ".\Visual Studio\VB\Resources\maps"

:AfterMkDir2

echo Copying Resources
copy ..\Base\All\Resources\* ".\Visual Studio\C#\Resources"
copy ..\Base\All\Resources\fonts\* ".\Visual Studio\C#\Resources\fonts"
copy ..\Base\All\Resources\images\* ".\Visual Studio\C#\Resources\images"
copy ..\Base\All\Resources\sounds\* ".\Visual Studio\C#\Resources\sounds"

copy ..\Base\All\Resources\* ".\Visual Studio\VB\Resources"
copy ..\Base\All\Resources\fonts\* ".\Visual Studio\VB\Resources\fonts"
copy ..\Base\All\Resources\images\* ".\Visual Studio\VB\Resources\images"
copy ..\Base\All\Resources\sounds\* ".\Visual Studio\VB\Resources\sounds"

echo Copying Code
copy ..\Base\DOTNet\C#\*.cs ".\Visual Studio\C#"
copy ..\Base\DOTNet\C#\*.csproj ".\Visual Studio\C#"

echo Copying Code
copy ..\Base\DOTNet\VB\*.vb ".\Visual Studio\VB"
copy ..\Base\DOTNet\VB\*.vbproj ".\Visual Studio\VB"

if not exist ".\Visual Studio\C#\Properties" mkdir ".\Visual Studio\C#\Properties"

copy ..\Base\DOTNet\Properties\* ".\Visual Studio\C#\Properties"