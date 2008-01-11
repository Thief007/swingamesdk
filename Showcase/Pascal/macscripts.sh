#!/bin/sh

Usage()
{
	echo "Usage: $0 [-c] [-h]"
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

if [ -d ./lib ] 
then
	echo "Removing old lib dir"
	rm -rf ./lib
fi

if [ $CLEAN = "Y" ]
then
	echo "Removing build script"
	rm unxbuild.sh
	exit 0
fi

echo "Making new lib folder"
mkdir ./lib

LIB_PATH=../../SDKs/Pascal/Mac/FPC

echo "Copying lib from ${LIB_PATH}"
cp  ${LIB_PATH}/lib/* ./lib
cp  ${LIB_PATH}/macbuild.sh .