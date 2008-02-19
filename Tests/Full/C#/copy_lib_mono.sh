#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd $APP_PATH

echo __________________________________________________
echo Copying Mono Files
echo __________________________________________________

if [ -d ./lib ] 
then
	echo "  Removing old lib dir"
	rm -rf ./lib
fi

echo "  Making new lib folder"
mkdir ./lib

LIB_PATH=../../../SDKs/DOTNet/Mono/C#

echo "  Copying lib from ${LIB_PATH}"
cp  -R ${LIB_PATH}/lib/* ./lib
cp  ${LIB_PATH}/*.sh .

echo "  Copying Resources"

SOURCE_RESOURCE=../Pascal/Resources
RESOURCE_DIR=./Resources

if [ ! -d ${RESOURCE_DIR} ]
then
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
fi

#cp ${SOURCE_RESOURCE}/* ${RESOURCE_DIR}
find ${SOURCE_RESOURCE} -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR} \;
if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi
	 
#cp ${SOURCE_RESOURCE}/fonts/* ${RESOURCE_DIR}/fonts
find ${SOURCE_RESOURCE}/fonts -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/fonts \;
if [ $? != 0 ]; then echo "Error copying fonts"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/images/* ${RESOURCE_DIR}/images
find ${SOURCE_RESOURCE}/images -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/images \;
if [ $? != 0 ]; then echo "Error copying images"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/sounds/* ${RESOURCE_DIR}/sounds
find ${SOURCE_RESOURCE}/sounds -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/sounds \;
if [ $? != 0 ]; then echo "Error copying sounds"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/maps/* ${RESOURCE_DIR}/maps	
find ${SOURCE_RESOURCE}/maps -maxdepth 1 -type f -exec cp {} ${RESOURCE_DIR}/maps \;
if [ $? != 0 ]; then echo "Error copying maps"; exit 1; fi


echo "  Finished"
echo __________________________________________________
