#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd $APP_PATH

Usage()
{
	echo "Usage: $0 [-l lib] [-c] [-h] DemoName"
	exit 1
}

CLEAN="N"

while getopts l:ch o
do
	case "$o" in
	h)  Usage;;
	l)	LIB="$OPTARG";;
	c)  CLEAN="Y";;
	esac
done

shift $((${OPTIND}-1))

if [ -n "$1" ] 
then
	DEMO_NAME="$1"
else
	Usage
fi

echo "__________________________________________________"
echo " Copying Mac and Unix version"
echo "__________________________________________________"
echo " Copying to ${DEMO_NAME}"
echo "__________________________________________________"
cd $DEMO_NAME

if [ -d ./lib ] 
then
	echo "  ... Removing old lib dir"
	rm -rf ./lib
fi

if [ $CLEAN = "Y" ]
then
	echo " ... Removing build script"
	rm -f build.sh
	exit 0
fi

echo " ... Making new lib folder"
mkdir ./lib

#
# Copy the library
#
if [ -z $LIB ]
then
	LIB="FPC"
fi

LIB_PATH=../../../SDKs/Pascal/Unx/${LIB}

if [ ! -d ${LIB_PATH} ]
then
	echo "ERROR: Cannot find library to copy"
    echo " ${LIB_PATH}"
	exit 1;
fi

LIB_PATH=`cd ${LIB_PATH}; pwd`

echo " ... Copying lib from ${LIB_PATH}"
# old: find ${LIB_PATH}/lib -maxdepth 1 -type f -exec cp {} ./lib \;
cp -R ${LIB_PATH}/lib/* ./lib
cp    ${LIB_PATH}/build.sh .

echo "  Finished"
echo "__________________________________________________"


