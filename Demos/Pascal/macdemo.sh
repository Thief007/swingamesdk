#!/bin/sh

Usage()
{
	echo "Usage: $0 [-l lib] [-c] [-h] DemoName"
	exit 1
}

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

cd $DEMO_NAME

if [ -d ./lib ] 
then
	echo "Removing old lib dir"
	rm -rf ./lib
fi

if [ $CLEAN = "Y" ]
then
	echo "Removing build script"
	rm macbuild.sh
	exit 0
fi

echo "Making new lib folder"
mkdir ./lib

#
# Copy the library
#
if [ -z $LIB ]
then
	LIB="FPC"
fi

LIB_PATH=../../../SDKs/Pascal/Mac/${LIB}

echo "Copying lib from ${LIB_PATH}"
cp  ${LIB_PATH}/lib/* ./lib
cp  ${LIB_PATH}/macbuild.sh .



