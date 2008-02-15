#!/bin/sh

#
# Compiles the library and copys into the FPC
# SDK folder.
#

DoExitCompile ()
{ 
	echo "An error occurred while compiling"; 
	cat ${BASEDIR}/out.log
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

BASEDIR=`pwd`
CLEAN="N"
SHARED_LIB="N"
EXTRA_OPTS="-O3 -Sewn -vwn"
DEBUG="N"

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
rm -rf "${FPCDir}"
mkdir -p "${FPCDir}"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	Output=`pwd`/bin/mac/	
	LibDir=`pwd`/lib/mac
	
	if [ $DEBUG = "Y" ]
	then
		EXTRA_OPTS="-gw -gl -gp -Ci -Co -Ct -vwnh "
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
		EXTRA_OPTS="-g -Ci -Co -Ct -vwnh "
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
	#
	# Compile and copy to SDKs
	#

	if [ ! -d $Output ]
	then
		echo "  ... Creating Output Location"
		mkdir -p "$Output"
	fi

	if [ -f ${BASEDIR}/out.log ]
	then
		rm -f ${BASEDIR}/out.log
	fi

	echo "  ... Compiling Core"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" SGSDK_Core.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Core"; cat ${BASEDIR}/out.log; exit 1; fi

	echo "  ... Compiling KeyCodes"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output" SGSDK_KeyCodes.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling KeyCodes"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Shapes"	
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Shapes.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Shapes"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Camera"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Camera.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Camera"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Graphics"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Graphics.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Graphics"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Font"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Font.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Font"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Input"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Input.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Input"; cat ${BASEDIR}/out.log;  cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Physics"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Physics.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Physics"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Audio"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_Audio.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Audio"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling MappyLoader"
	fpc -Mdelphi $EXTRA_OPTS -FE"$Output" -Fu"$Output"  SGSDK_MappyLoader.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling MappyLoader"; cat ${BASEDIR}/out.log;  exit 1; fi
	
	echo "  ... Output in ${BASEDIR}/out.log"		
	echo "  ... Copying to FPC"

	cp "$Output"/*.ppu "$FPCDir"
	cp "$Output"/*.o "$FPCDir"
	
	if [ ! -z $LibDir ]
	then
		echo "  ... Copying lib files"
		
		if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
		then
			pushd . > /dev/null
			cd "${LibDir}"
			
			#
			# Copying frameworks ... files to SDK
			#
			find . -type d \! -path *.svn* \! -name . -exec mkdir "${FPCDir}/{}" \;
			if [ $? != 0 ]; then echo "Error creating framework directories"; cat ${BASEDIR}/out.log;  exit 1; fi
			
			find . \! -type d \! -path *.svn* -exec cp -R {} "${FPCDir}/{}" \;
			if [ $? != 0 ]; then echo "Error copying framework files"; cat ${BASEDIR}/out.log;  exit 1; fi
								
			popd > /dev/null
		fi
	fi
fi

echo "  Finished"
echo "  Copied to ${FPCDir}"
echo "__________________________________________________"