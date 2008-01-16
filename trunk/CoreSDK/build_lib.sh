#!/bin/sh

Usage()
{
	echo Usage: [-c] [-h] [-d]
	echo 
	echo Compiles the Pascal version of the SwinGame API.
	echo
	echo Options:
	echo  -c   Perform a clean rather than a build
	echo  -h   Show this help message
	echo  -d   Compile with debug options on 
	exit 0
}

CLEAN="N"
SHARED_LIB="N"
EXTRA_OPTS="-O3"
DEGUB="N"

while getopts hdc o
do
	case "$o" in
	d)  DEBUG="Y";;
	h)  Usage;;
	c)  CLEAN="Y";;
	esac
done

shift $((${OPTIND}-1))

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	FPCDir=`pwd`/../SDKs/Pascal/Mac/FPC/lib/
	Output=`pwd`/bin/mac/
	LibDir=`pwd`/lib/mac

	if [ DEBUG = "Y" ]
	then
		EXTRA_OPTS="-gw3 -gl -gp -godwarfsets -Ci -Co -Ct"
	fi

	echo __________________________________________________
	echo Building Mac version
	echo __________________________________________________	
	echo "   Output to $Output"
	echo "   Copying to $FPCDir"
	echo "   Copying library from $LibDir"
	echo "   Compiling with " $EXTRA_OPTS

else
	Output=`pwd`/bin/unx/
	UnxFPCDir=`pwd`/../SDKs/Pascal/Unx/FPC/lib/
	LibDir=

	if [ DEBUG = "Y" ]
	then
		EXTRA_OPTS="-g -Ci -Co -Ct"
	fi
	
	echo __________________________________________________
	echo Building Linux version
	echo __________________________________________________	
	echo "   Output to $Output"
	echo "   Copying to $FPCDir"
	echo "   Compiling with " $EXTRA_OPTS
fi

cd src/

if [ $CLEAN = "Y" ]
then
	echo   ... Cleaning
	rm -rf "$Output"
	mkdir -p "$Output"
else
	echo   ... Creating Output Location
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
			
	echo "   ... Copying to FPC"

	mkdir -p "$FPCDir"

	cp "$Output"/*.ppu "$FPCDir"
	cp "$Output"/*.o "$FPCDir"
	
	if [ ! -z $LibDir ]
	then
		echo "   ... Copying lib files"
		cp "$LibDir"/*.a "$FPCDir"
	fi
fi

echo "   Finished"