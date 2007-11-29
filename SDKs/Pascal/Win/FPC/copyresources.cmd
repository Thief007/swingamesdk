@echo off
REM Used to copy resources within Delphi

if not exist .\bin mkdir .\bin
if not exist .\bin\resources mkdir .\bin\resources


echo ************************
echo Copying libraries to bin
echo ************************

copy .\lib\*.dll .\bin
echo ... done

echo ************************
echo Copying resources to bin directory
echo ************************

cp -R -u .\resources\* .\bin\resources
echo ... done

