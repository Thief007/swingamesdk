@echo off

echo __________________________________________________
echo Building c# SwinGame
echo __________________________________________________
echo   Building %1%
echo __________________________________________________

if exist out.log del /q out.log

msbuild >> out.log
if ERRORLEVEL 1 goto error

echo   Finished
echo __________________________________________________

goto end

:error
	echo An error occurred
	type out.log
:end 