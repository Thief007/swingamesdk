#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

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

DoExitAsm ()
{ 
	echo "An error occurred while assembling $1"; 
	exit 1; 
}

DoCompile()
{
	echo "  ... Compiling to $1"
	
	if [ ! -d $1 ]
	then
		echo "  ... Creating Output Location"
		mkdir -p "$1"
	fi
	
	echo "  ... Compiling Core"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1" SGSDK_Core.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Core"; cat ${BASEDIR}/out.log; exit 1; fi

	echo "  ... Compiling Shapes"	
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Shapes.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Shapes"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Camera"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Camera.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Camera"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Graphics"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Graphics.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Graphics"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Font"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Font.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Font"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Input"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Input.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Input"; cat ${BASEDIR}/out.log;  cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Physics"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Physics.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Physics"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling Audio"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_Audio.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling Audio"; cat ${BASEDIR}/out.log;  exit 1; fi

	echo "  ... Compiling MappyLoader"
	${FPC_BIN} -Mdelphi $EXTRA_OPTS -FE"$1" -Fu"$1"  SGSDK_MappyLoader.pas >> ${BASEDIR}/out.log
	if [ $? != 0 ]; then echo "Error compiling MappyLoader"; cat ${BASEDIR}/out.log;  exit 1; fi
}

DoAssemble()
{
	echo "  ... Assembling Manually - arch $2"
	pushd . >> /dev/null
	cd "$1"
	
	#Remove the pascal assembler script
	rm ppas.sh

	#Assemble all of the .s files
	for file in `find . | grep [.]s$` #`ls *.s`
	do
		/usr/bin/as -o ${file%.s}.o $file -arch $2
		if [ $? != 0 ]; then DoExitAsm $file; fi
		rm $file
	done
	
	popd >> /dev/null
}

DoCopy()
{
	echo "  ... Getting from $1"
	echo "  ... Copying to $2"
	
	if [ ! -d $2 ]
	then
		mkdir -p $2
	fi

	cp "${1}"/*.ppu ${2}/
	cp "${1}"/*.o ${2}/
}

BASEDIR=`pwd`
CLEAN="N"
SHARED_LIB="N"
EXTRA_OPTS="-O3 -Sewn -vwn"
DEBUG="N"
FPC_BIN=`which fpc`

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
	Output=`pwd`/bin/mac	
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
	Output=`pwd`/bin/unx
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

if [ -f ${BASEDIR}/out.log ]
then
	rm -f ${BASEDIR}/out.log
fi

if [ $CLEAN = "Y" ]
then
	echo "  ... Cleaning"
	rm -rf "$Output"
	mkdir -p "$Output"
else	
	#
	# Compile and copy to SDKs
	#

	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		rm ${Output}/* 2>> /dev/null
		
		FPC_BIN=`which ppc386`
		DoCompile ${Output}/i386
		DoCopy "${Output}/i386" "${FPCDir}/i386" 
		echo "  ________________________________________________"
			
		FPC_BIN=`which ppcppc`
		EXTRA_OPTS="-s $EXTRA_OPTS"
		DoCompile ${Output}/ppc
		DoAssemble ${Output}/ppc ppc
		DoCopy "${Output}/ppc" "${FPCDir}/ppc" 
		echo "  ________________________________________________"
	else
		DoCompile "$Output"
		DoCopy "$Output" "$FPCDir" 
	fi
	
	echo "  ... Output in ${BASEDIR}/out.log"		
	
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