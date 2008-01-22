#!/bin/sh

echo "__________________________________________________"
echo "Copying Mono Files"
echo "__________________________________________________"
	
# Copy all files from the Base folder to Mono.
find ../Base/Al -maxdepth 1 -type f -exec cp {} ./Mono/C# \;

if [ -d ./Mono/C#/Resources ]
then
	echo "  ... Removing Old Resources Directory"
	rm -rf ./Mono/C#/Resources
fi

echo "  ... Creating Resources Directory"
mkdir ./Mono/C#/Resources
mkdir ./Mono/C#/Resources/fonts
mkdir ./Mono/C#/Resources/images
mkdir ./Mono/C#/Resources/sounds
mkdir ./Mono/C#/Resources/maps

echo "  ... Copying Resources"

SOURCE_RESOURCE=../Base/All/Resources
RESOURCE_DIR=./Mono/C#/Resources

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

echo "  ... Copying Code"
cp ../Base/DOTNet/*.cs ./Mono/C#
cp ../Base/DOTNet/*.csproj ./Mono/C#

if [ ! -d ./Mono/C#/Properties ]
then
	mkdir ./Mono/C#/Properties
fi
cp ../Base/DOTNet/Properties/* ./Mono/C#/Properties


echo "  Finished"
echo "__________________________________________________"