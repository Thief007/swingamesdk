#!/bin/sh
Output=`pwd`/bin/mac/
cd lib/Pas/
if [ $1 = "-clean" ]
then
	cd "$Output"
	rm -f SGSDK_Core.*
	rm -f SGSDK_Graphics.*
	rm -f SGSDK_Font.*
	rm -f SGSDK_Input.*
	rm -f SGSDK_Physics.*
	rm -f SGSDK_Audio.*
	rm -f sdl_image.*
	rm -f sdl_mixer.*
	rm -f sdl_ttf.*
	rm -f sdl.*
	rm -f SDLEventProcessing.*
	echo Cleaned
else
	mkdir -p "$Output"
	fpc -Mdelphi -FE"$Output" SGSDK_Core.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Graphics.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Font.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Input.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Physics.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Audio.pas
fi
#open "$Output"