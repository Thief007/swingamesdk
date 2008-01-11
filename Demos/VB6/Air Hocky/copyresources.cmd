@echo off
REM Used to copy resources for VB6

if not exist .\bin mkdir .\bin
if not exist .\bin\resources mkdir .\bin\resources


echo ************************
echo Copying libraries to bin
echo ************************

Xcopy .\lib\*.dll .\bin /s/y/d
copy .\lib\*.tlb .\bin
echo ... done

echo ************************
echo Copying resources to bin directory
echo ************************

Xcopy .\resources\* .\bin\resources /s/y/d

echo ************************
echo Copying executable to bin
echo ************************
copy .\*.exe .\bin
del .\*.exe
copy .\SGSDKVB6.reg .\bin

echo ... done

