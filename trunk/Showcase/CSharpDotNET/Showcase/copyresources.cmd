@echo off
REM Used to copy resources within Delphi

if not exist .\bin\Debug mkdir .\bin\Debug
if not exist .\bin\Debug\resources mkdir .\bin\Debug\resources
if not exist .\bin\Release mkdir .\bin\Release
if not exist .\bin\Release\resources mkdir .\bin\Release\resources


echo ************************
echo Copying resources to bin directory
echo ************************

cp -R -u .\resources\* .\bin\Debug\resources
cp -R -u .\resources\* .\bin\Release\resources
echo ... done

