#!/bin/sh

BaseDir=`pwd`
echo Running script from $BaseDir

Output="$BaseDir"/SGSDK.NET/lib
echo Saving output to $Output

DOTNETlocn="$BaseDir"/SGSDK.NET/src/
DOTNETproj=SGSDK.NET.csproj
DOTNETbin="$BaseDir"/SGSDK.NET/src/bin/Debug

SDK="$BaseDir"/../SDKs/DOTNet/Mono/C#/lib/

CLEAN="N"

while getopts c o
do
	case "$o" in
	c)  CLEAN="Y";;
	esac
done

if [ $CLEAN = "N" ]
then
	fpc -v0 -g -Mdelphi -FE"$Output" ./src/SGSDK.pas
	
	rm "$Output"/*.o
	rm "$Output"/*.ppu

	cd $DOTNETlocn
	xbuild $DOTNETproj

	cp "$DOTNETbin"/*.dll "$Output"

	echo Copying to C# SDK
	cp "$Output"/*.dll "$SDK"
	cp "$Output"/*.so  "$SDK"

	echo Finished
else
	rm "$Output"/*.dll
	rm "$Output"/*.so
	echo Cleaned
fi