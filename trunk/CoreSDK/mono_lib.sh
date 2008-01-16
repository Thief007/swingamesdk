#!/bin/sh

DoExitAsm ()
{ 
	echo "An error occurred while assembling $1" 
	exit 1 
}

Usage()
{
	echo Usage: [-c] [-h]
	echo 
	echo Compiles the SGSDK library and the SGSDK.NET library.
	echo Libraries are copied to the Mono SDK folder.
	echo
	echo Options:
	echo  -c   Perform a clean rather than a build
	echo  -h   Show this help message 
	exit 0
}

BaseDir=`pwd`
Output="$BaseDir"/SGSDK.NET/lib
	
DOTNETlocn="$BaseDir"/SGSDK.NET/src/
DOTNETproj=SGSDK.NET.csproj
DOTNETbin="$BaseDir"/SGSDK.NET/src/bin/Debug

SDK="$BaseDir"/../SDKs/DOTNet/Mono/C#/lib/

EXTRA_OPTS="-O3"

CLEAN="N"

while getopts ch o
do
	case "$o" in
	c)  CLEAN="Y";;
	h)  Usage();;
	esac
done

shift $((${OPTIND}-1))

if [ $CLEAN = "N" ]
then
	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		echo __________________________________________________
		echo Building Mac version
		echo __________________________________________________
		echo   Running script from $BaseDir
		echo   Saving output to $Output

		echo   ... Compiling Library
	
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -FU"$Output" -s ./src/SGSDK.pas
		if [ $? != 0 ]; then echo "Error compiling SGSDK"; exit 1; fi
	
		#Assemble all of the .s files
		echo __________________________________________________
		echo   ... Assembling library

		for file in `find $Output | grep [.]s$` #`ls *.s`
		do
			echo      - $file
			/usr/bin/as -o ${file%.s}.o $file -arch i386
			if [ $? != 0 ]; then DoExitAsm $file; fi
			rm $file
		done

		echo __________________________________________________
		echo   ... Linking Library
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
		echo   ... Copying Library to "$Output"
		cp "$DOTNETbin"/*.dll "$Output"
		cp "$DOTNETbin"/*.xml "$Output"
		rm "$DOTNETbin/*"

		echo   ... Copying to C# SDK
		cp "$Output"/*.dll "$SDK"
		cp "$Output"/*.xml "$SDK"
		cp "$Output"/*.dylib  "$SDK"
	else
		echo __________________________________________________
		echo Building Linux version
		echo __________________________________________________
		echo   Running script from $BaseDir
		echo   Saving output to $Output

		fpc -v0 -g -Mdelphi $EXTRA_OPTS -FE"$Output" ./src/SGSDK.pas

		rm "$Output"/*.o
		rm "$Output"/*.ppu

		cd $DOTNETlocn
		xbuild $DOTNETproj

		cp "$DOTNETbin"/*.dll "$Output"

		echo Copying to C# SDK
		cp "$Output"/*.dll "$SDK"
		cp "$Output"/*.so  "$SDK"	
	fi
else
	rm -rf "$Output"
	mkdir "$Output"
	echo    ... Cleaned
fi

echo Finished