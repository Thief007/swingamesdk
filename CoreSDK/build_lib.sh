#!/bin/sh

DoExitCompile ()
{ 
	echo "An error occurred while compiling"; 
	cat out.log
	exit 1; 
}

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

FPCDir=`pwd`/../SDKs/Pascal/Unx/FPC/lib/
if [ ! -d $FPCDir ]
then
	mkdir -p $FPCDir
fi
FPCDir=`cd $FPCDir; pwd`


if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	Output=`pwd`/bin/mac/	
	LibDir=`pwd`/lib/mac
	
	if [ DEBUG = "Y" ]
	then
		EXTRA_OPTS="-gw3 -gl -gp -godwarfsets -Ci -Co -Ct"
	fi

	echo "__________________________________________________"
	echo "Building Mac version"
	echo "__________________________________________________"
	echo "  Output to:        $Output"
	echo "  Copying to:       $FPCDir"
	echo "  Copying Library:  $LibDir"
	echo "  Compiler Options: $EXTRA_OPTS"
	echo "__________________________________________________"
	
else
	Output=`pwd`/bin/unx/
	Output=`cd $Output; pwd`
	
	LibDir=

	if [ DEBUG = "Y" ]
	then
		EXTRA_OPTS="-g -Ci -Co -Ct"
	fi
	
	echo "__________________________________________________"
	echo "Building Linux version"
	echo "__________________________________________________"	
	echo "  Output to:        $Output"
	echo "  Copying to:       $FPCDir"
	echo "  Compiler Options: $EXTRA_OPTS"
	echo "__________________________________________________"
fi

cd src/

if [ $CLEAN = "Y" ]
then
	echo "  ... Cleaning"
	rm -rf "$Output"
	mkdir -p "$Output"
else	
	if [ ! -d $Output ]
	then
		echo "  ... Creating Output Location"
		mkdir -p "$Output"
	fi

	if [ -f out.log ]
	then
		rm -f out.log
	fi

	echo "  ... Compiling Core"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Core.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Core"; cat out.log; exit 1; fi

	echo "  ... Compiling KeyCodes"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_KeyCodes.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling KeyCodes"; cat out.log;  exit 1; fi

	echo "  ... Compiling Shapes"	
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Shapes.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Shapes"; cat out.log;  exit 1; fi

	echo "  ... Compiling Camera"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Camera.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Camera"; cat out.log;  exit 1; fi

	echo "  ... Compiling Graphics"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Graphics.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Graphics"; cat out.log;  exit 1; fi

	echo "  ... Compiling Font"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Font.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Font"; cat out.log;  exit 1; fi

	echo "  ... Compiling Input"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Input.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Input"; cat out.log;  cat out.log;  exit 1; fi

	echo "  ... Compiling Physics"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Physics.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Physics"; cat out.log;  exit 1; fi

	echo "  ... Compiling Audio"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_Audio.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling Audio"; cat out.log;  exit 1; fi

	echo "  ... Compiling MappyLoader"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" -Sewn -vwn SGSDK_MappyLoader.pas > out.log
	if [ $? != 0 ]; then echo "Error compiling MappyLoader"; cat out.log;  exit 1; fi
	
	echo "  ... Output in out.log"		
	echo "  ... Copying to FPC"

	cp "$Output"/*.ppu "$FPCDir"
	cp "$Output"/*.o "$FPCDir"
	
	if [ ! -z $LibDir ]
	then
		echo "  ... Copying lib files"
		cp "$LibDir"/*.a "$FPCDir"
	fi
fi

echo "  Finished"
echo "  Copied to ${FPCDir}"
echo "__________________________________________________"