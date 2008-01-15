#!/bin/sh

DoExitAsm ()
{ 
	echo "An error occurred while assembling $1" 
	exit 1 
}


BaseDir=`pwd`
echo Running script from $BaseDir

Output="$BaseDir"/SGSDK.NET/lib
echo Saving output to $Output
echo __________________________________________________
	
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
	echo Compiling Library
	
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -FU"$Output" -s ./src/SGSDK.pas
	if [ $? != 0 ]; then echo "Error compiling SGSDK"; exit 1; fi
	
	#Assemble all of the .s files
	echo __________________________________________________
	echo Assembling library

	for file in `find $Output | grep [.]s$` #`ls *.s`
	do
		echo ... $file
		/usr/bin/as -o ${file%.s}.o $file -arch i386
		if [ $? != 0 ]; then DoExitAsm $file; fi
		rm $file
	done

	echo __________________________________________________
	echo Linking Library
	/usr/bin/libtool  -dynamic -L./lib/mac -L"$Output" -search_paths_first -multiply_defined suppress -o "$Output/libSGSDK.dylib" `cat ./src/maclink.res`
	if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
	
	rm "$Output"/*.o
	rm "$Output"/*.ppu
	rm "$Output"/link.res
	rm "$Output"/ppas.sh

	cd $DOTNETlocn
	echo __________________________________________________
	xbuild $DOTNETproj
	if [ $? != 0 ]; then echo "Error with xbuild"; exit 1; fi
			
	echo __________________________________________________
	echo Copying Library to "$Output"
	cp "$DOTNETbin"/*.dll "$Output"
	cp "$DOTNETbin"/*.xml "$Output"
	rm "$DOTNETbin/*"

	echo Copying to C# SDK
	cp "$Output"/*.dll "$SDK"
	cp "$Output"/*.xml "$SDK"
	cp "$Output"/*.dylib  "$SDK"

	echo Finished
else
	rm "$Output"/*.dll
	rm "$Output"/*.xml
	rm "$Output"/*.dylib
	echo Cleaned
fi