#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

Usage()
{
	echo "Usage: $0 [-h] [-r]"
	echo 
	echo "Build the mono version of a SwinGame " 
	echo
	echo "Options:"
	echo " -h = Show usage" 
	echo " -r = Use Release build"
	exit 1
}

BUILD="DEBUG"

while getopts hr o
do
	case "$o" in
	h)  Usage;;
	r)  BUILD="RELEASE";;
	esac
done

shift $((${OPTIND}-1))

if [ $BUILD = "DEBUG" ]
then
	BIN_DIR="./bin/Debug"
else
	BIN_DIR="./bin/Release"
fi

if [ ! -d ${BIN_DIR} ]
then
	mkdir -p ${BIN_DIR}
fi


if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	echo "__________________________________________________"
	echo "Building Mac version - $BUILD"
	echo "__________________________________________________"
	echo " Running vbnc"

	FRAMEWORK_DIR=./bin/Frameworks
	
	if [ -d $FRAMEWORK_DIR ]
	then
		rm -rf "$FRAMEWORK_DIR"
	fi
	mkdir -p $FRAMEWORK_DIR
	cp -R ./lib/*.framework $FRAMEWORK_DIR

	RESOURCE_DIR=./bin/Resources
else
	echo "__________________________________________________"
	echo "Building Linux version - $BUILD"
	echo "__________________________________________________"
	echo " Running xbuild $1 ${BUILD_OPT}"

	RESOURCE_DIR=${BIN_DIR}/Resources
fi

	vbnc /noconfig /debug:full /debug+ /out:${BIN_DIR}/GameProject.exe *.vb ./Model/*.vb "My Project/AssemblyInfo.vb" /target:winexe /win32icon:SwinGame.ico  /define:CONFIG="Debug",DEBUG=-1,TRACE=-1,_MyType="WindowsFormsWithCustomSubMain",PLATFORM="AnyCPU" /imports:SwinGame /imports:Microsoft.VisualBasic /imports:System /imports:System.Collections /imports:System.Collections.Generic /imports:System.Drawing /imports:System.Diagnostics /main:Battleships.GameLogic /rootnamespace:Battleships /r:lib/SGSDK.NET.dll /r:System.Drawing.dll /r:System.Windows.Forms.dll /r:System.dll > out.log
	if [ $? != 0 ]; then echo "Error with vbnc"; cat out.log; exit 1; fi
	
echo "  ... Copying Library Files"
cp ./lib/*.dll ${BIN_DIR}
if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	cp ./lib/*.dylib ${BIN_DIR}
	if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
else
	cp ./lib/*.so ${BIN_DIR}
	if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
fi

SOURCE_RESOURCE=./Resources

if [ -d ${RESOURCE_DIR} ]
then
	echo "  ... Removing Old Resources"
	rm -rf ${RESOURCE_DIR}
fi

echo "  ... Copying Resources"

mkdir ${RESOURCE_DIR}
if [ $? != 0 ]; then echo "Error creating resource directory"; exit 1; fi
	
mkdir ${RESOURCE_DIR}/fonts
if [ $? != 0 ]; then echo "Error creating fonts directory"; exit 1; fi
	
mkdir ${RESOURCE_DIR}/images
if [ $? != 0 ]; then echo "Error creating images directory"; exit 1; fi
	
mkdir ${RESOURCE_DIR}/sounds
if [ $? != 0 ]; then echo "Error creating sounds directory"; exit 1; fi
	
mkdir ${RESOURCE_DIR}/maps
if [ $? != 0 ]; then echo "Error creating maps directory"; exit 1; fi

find ${SOURCE_RESOURCE} -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR} \;
if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi

find ${SOURCE_RESOURCE}/fonts -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/fonts \;
if [ $? != 0 ]; then echo "Error copying fonts"; exit 1; fi

find ${SOURCE_RESOURCE}/images -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/images \;
if [ $? != 0 ]; then echo "Error copying images"; exit 1; fi

find ${SOURCE_RESOURCE}/sounds -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/sounds \;
if [ $? != 0 ]; then echo "Error copying sounds"; exit 1; fi

find ${SOURCE_RESOURCE}/maps -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/maps \;
if [ $? != 0 ]; then echo "Error copying maps"; exit 1; fi

echo "  Build to: ${BIN_DIR}"
echo "  Finished"
echo "__________________________________________________"
