#!/bin/sh

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	echo __________________________________________________
	echo Building Mac version
	echo __________________________________________________
	
	xbuild $1
	if [ $? != 0 ]; then echo "Error with xbuild"; exit 1; fi
	echo __________________________________________________
	
	echo Copying Library Files
	cp ./lib/* ./bin/Debug
	if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi
			
	echo Copying Resources
	cp -R ./Resources ./bin
	if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi
else
	echo __________________________________________________
	echo Building Linux version
	echo __________________________________________________
	
	xbuild $1
	if [ $? != 0 ]; then echo "Error with xbuild"; exit 1; fi
	echo __________________________________________________

	echo Copying Library Files
	cp ./lib/* ./bin/Debug
	if [ $? != 0 ]; then echo "Error copying library"; exit 1; fi

	echo Copying Resources
	#cp -R ./Resources ./bin/Debug
	
	RESOURCE_DIR=./bin/Debug/Resources
	SOURCE_RESOURCE=../Base/All/Resources
	
	mkdir ${RESOURCE_DIR}
	mkdir ${RESOURCE_DIR}/fonts
	mkdir ${RESOURCE_DIR}/images
	mkdir ${RESOURCE_DIR}/sounds
	mkdir ${RESOURCE_DIR}/maps

	cp ${SOURCE_RESOURCE}/* ${RESOURCE_DIR} 
	cp ${SOURCE_RESOURCE}/fonts/* ${RESOURCE_DIR}/fonts
	cp ${SOURCE_RESOURCE}/images/* ${RESOURCE_DIR}/images
	cp ${SOURCE_RESOURCE}/sounds/* ${RESOURCE_DIR}/sounds
	cp ${SOURCE_RESOURCE}/sounds/* ${RESOURCE_DIR}/maps	
	
	if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi
fi