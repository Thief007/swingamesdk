#!/bin/sh

Output=`pwd`/bin/mac/
MacFPCDir=`pwd`/../SDKs/Pascal/Mac/FPC/lib/
LibDir=`pwd`/lib/mac

cd src/

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
	rm -f SGSDK_KeyCodes.*
	echo Cleaned
else
	mkdir -p "$Output"
	fpc -Mdelphi -FE"$Output" SGSDK_Core.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Graphics.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Font.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Input.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Physics.pas
	fpc -Mdelphi -FE"$Output" SGSDK_Audio.pas
	fpc -Mdelphi -FE"$Output" SGSDK_KeyCodes.pas
	echo Copying to FPC
	cp "$Output"/*.ppu "$MacFPCDir"
	cp "$Output"/*.o "$MacFPCDir"
	cp "$LibDir"/*.a "$MacFPCDir"
	echo Finished
fi
#open "$Output"