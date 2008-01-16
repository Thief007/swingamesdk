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

echo Copying Resources
copy ..\Base\All\Resources\* ".\Visual Studio\C#\Resources"
copy ..\Base\All\Resources\fonts\* ".\Visual Studio\C#\Resources\fonts"
copy ..\Base\All\Resources\images\* ".\Visual Studio\C#\Resources\images"
copy ..\Base\All\Resources\sounds\* ".\Visual Studio\C#\Resources\sounds"

echo Copying Code
copy ..\Base\DOTNet\*.cs ".\Visual Studio\C#"
copy ..\Base\DOTNet\*.csproj ".\Visual Studio\C#"

if not exist ".\Visual Studio\C#\Properties" mkdir ".\Visual Studio\C#\Properties"

copy ..\Base\DOTNet\Properties\* ".\Visual Studio\C#\Properties"