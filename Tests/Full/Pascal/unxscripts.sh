#!/bin/sh

Usage()
{
	echo "Usage: $0 [-c] [-h]"
	echo
	echo "Copies the scripts and library from the current core SDK"
	exit 1
}

CLEAN="N"

while getopts ch o
do
	case "$o" in
	h) 	Usage;;
	c)	CLEAN="Y";;
	esac
done

shift $((${OPTIND}-1))

echo "__________________________________________________"
echo "Copying Mac/Linux Scripts"
echo "__________________________________________________"

if [ -d ./lib ] 
then
	echo "  ... Removing old lib dir"
	rm -rf ./lib
fi

if [ $CLEAN = "Y" ]
then
	echo "  ... Removing build script"
	rm unxbuild.sh
	exit 0
fi

echo "  ... Making new lib folder"
mkdir ./lib

LIB_PATH=../../../SDKs/Pascal/Unx/FPC
LIB_PATH=`cd ${LIB_PATH}; pwd`

echo "  ... Copying lib from ${LIB_PATH}"

cp  -R ${LIB_PATH}/lib/* ./lib
cp  ${LIB_PATH}/build.sh .

echo "  Finished"
echo "__________________________________________________"