#!/bin/sh

DoExitAsm ()
{ 
	echo "An error occurred while assembling $1" 
	exit 1 
}

Usage()
{
	echo "Usage: [-c] [-h]"
	echo 
	echo "Compiles the SGSDK library and the SGSDK.NET library."
	echo "Libraries are copied to the Mono SDK folder."
	echo
	echo "Options:"
	echo " -c   Perform a clean rather than a build"
	echo " -h   Show this help message "
	exit 0
}

BaseDir=`pwd`
Output="$BaseDir"/SGSDK.NET/lib
	
DOTNETlocn="$BaseDir"/SGSDK.NET/src/
DOTNETproj=SGSDK.NET.csproj
DOTNETbin="$BaseDir"/SGSDK.NET/src/bin/Debug

SDKBase="${BaseDir}/../SDKs/DOTNet/Mono/"

EXTRA_OPTS="-O3 -Sewn -vwn"

CLEAN="N"

function cpToSDK
{
	echo "  ... Copying to $1 SDK"

	cp "$Output"/*.dll "$SDKBase/$1/lib"
			if [ $? != 0 ]; then echo "Error copying DLL"; exit 1; fi
	cp "$Output"/*.XML "$SDKBase/$1/lib"
			if [ $? != 0 ]; then echo "Error copying XML"; exit 1; fi

	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		cp "$Output"/*.dylib  "$SDKBase/$1/lib"
				if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
	fi
}

while getopts chd o
do
	case "$o" in
	c)  CLEAN="Y" ;;
	h)  Usage ;;
	d)  EXTRA_OPTS="-vwn";;
	esac
done

shift $((${OPTIND}-1))

if [ $CLEAN = "N" ]
then
	if [ -f ${BaseDir}/out.log ]
	then
		rm -f ${BaseDir}/out.log
	fi

	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		echo "__________________________________________________"
		echo "Building Mac version"
		echo "__________________________________________________"
		echo "  Running script from $BaseDir"
		echo "  Saving output to $Output"
		echo "__________________________________________________"

		echo "  ... Compiling Library"
	
		echo "Compiling with $EXTRA_OPTS" >> ${BaseDir}/out.log
	
		fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -FU"$Output" -s ./src/SGSDK.pas >> ${BaseDir}/out.log
		if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${BaseDir}/out.log; exit 1; fi
	
		#Assemble all of the .s files
		echo "  ... Assembling library from : "

		for file in `find $Output | grep [.]s$` #`ls *.s`
		do
			echo "  ... - $file"
			/usr/bin/as -o ${file%.s}.o $file -arch i386
			if [ $? != 0 ]; then DoExitAsm $file; fi
			rm $file
		done

		echo "  ... Linking Library"
		/usr/bin/libtool  -dynamic -L./lib/mac -L"$Output" -search_paths_first -multiply_defined suppress -o "$Output/libSGSDK.dylib" `cat ./src/maclink.res`
		if [ $? != 0 ]; then echo "Error linking"; exit 1; fi
	
		rm "$Output"/*.o
		rm "$Output"/*.ppu
		rm "$Output"/link.res
		rm "$Output"/ppas.sh

		cd $DOTNETlocn
		
		echo "  ... Compiling .NET Library"
		xbuild $DOTNETproj >> ${BaseDir}/out.log
		if [ $? != 0 ]; then echo "Error with xbuild"; cat ${BaseDir}/out.log; exit 1; fi
			
		echo "  ... Copying Library to $Output"
		cp "$DOTNETbin"/*.dll "$Output"
		cp "$DOTNETbin"/*.XML "$Output"
		rm "$DOTNETbin"/*
	else
		echo "__________________________________________________"
		echo "Building Linux version"
		echo "__________________________________________________"
		echo "  Running script from $BaseDir"
		echo "  Saving output to $Output"

		fpc -v0 -g -Mdelphi $EXTRA_OPTS -FE"$Output" ./src/SGSDK.pas

		rm "$Output"/*.o
		rm "$Output"/*.ppu

		cd $DOTNETlocn
		xbuild $DOTNETproj >> ${BaseDir}/out.log

		cp "$DOTNETbin"/*.dll "$Output"	
	fi
else
	rm -rf "$Output"
	mkdir "$Output"
	echo    ... Cleaned
fi

cpToSDK C#
cpToSDK VB

echo "  Finished"
echo "__________________________________________________"