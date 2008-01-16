#!/bin/sh

Usage()
{
	echo "Usage: $0 DemoName"
	exit 1
}

if [ -n "$1" ] 
then
	DEMO_NAME="$1"
else
	Usage
fi

cd $DEMO_NAME

echo __________________________________________________
echo Copying Mono Files
echo __________________________________________________

if [ -d ./lib ] 
then
	echo "   Removing old lib dir"
	rm -rf ./lib
fi

echo "   Making new lib folder"
mkdir ./lib

LIB_PATH=../../../SDKs/DOTNet/Mono/C#

echo "   Copying lib from ${LIB_PATH}"
cp  ${LIB_PATH}/lib/* ./lib
cp  ${LIB_PATH}/*.sh .

echo "   Finished"