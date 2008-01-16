@echo off

echo __________________________________________________
echo Copying Visual Studio Files
echo __________________________________________________

cp ..\Base\All\* ".\Visual Studio\C#"

if exist ".\Visual Studio\C#\Resources" goto AfterMkDir

echo Creating Resources Directory
mkdir ".\Visual Studio\C#\Resources"
mkdir ".\Visual Studio\C#\Resources\fonts"
mkdir ".\Visual Studio\C#\Resources\images"
mkdir ".\Visual Studio\C#\Resources\sounds"
mkdir ".\Visual Studio\C#\Resources\maps"

:AfterMkDir

echo Copying Resources
cp ..\Base\All\Resources\* ".\Visual Studio\C#\Resources"
cp ..\Base\All\Resources\fonts\* ".\Visual Studio\C#\Resources\fonts"
cp ..\Base\All\Resources\images\* ".\Visual Studio\C#\Resources\images"
cp ..\Base\All\Resources\sounds\* ".\Visual Studio\C#\Resources\sounds"

echo Copying Code
cp ..\Base\DOTNet\*.cs ".\Visual Studio\C#"
cp ..\Base\DOTNet\*.csproj ".\Visual Studio\C#"

if not exist ".\Visual Studio\C#\Properties" mkdir ./Mono/C#/Properties

cp ..\Base\DOTNet\Properties\* ".\Visual Studio\C#\Properties"