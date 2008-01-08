#!/bin/sh

Output=`pwd`/bin/unx/
UnxFPCDir=`pwd`/../SDKs/Pascal/Unx/FPC/lib/
CLEAN="N"
EXTRA_OPTS="-O3"

while getopts hdc o
do
	case "$o" in
	d)  EXTRA_OPTS="-g -gl -gc";;
	h)  Usage;;
	c)  CLEAN="Y"
	esac
done

cd src/

if [ $CLEAN = "Y" ]
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
	rm -f SGSDK_MappyLoader.*
	rm -f SGSDK_Camera.*
	echo Cleaned
else
	echo "Compiling with " $EXTRA_OPTS

	mkdir -p "$Output"
	
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Core.pas
	if [ $? != 0 ]; then echo "Error compiling Core"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Camera.pas
	if [ $? != 0 ]; then echo "Error compiling Camera"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Graphics.pas
	if [ $? != 0 ]; then echo "Error compiling Graphics"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Font.pas
	if [ $? != 0 ]; then echo "Error compiling Font"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Input.pas
	if [ $? != 0 ]; then echo "Error compiling Input"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Physics.pas
	if [ $? != 0 ]; then echo "Error compiling Physics"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_Audio.pas
	if [ $? != 0 ]; then echo "Error compiling Audio"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_KeyCodes.pas
	if [ $? != 0 ]; then echo "Error compiling KeyCodes"; exit 1; fi
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" SGSDK_MappyLoader.pas
	if [ $? != 0 ]; then echo "Error compiling MappyLoader"; exit 1; fi
	
	echo "Copying to FPC"
	
	mkdir -p "$UnxFPCDir"
	
	cp "$Output"/*.ppu "$UnxFPCDir"
	cp "$Output"/*.o "$UnxFPCDir"
	
	echo "Finished"
fi