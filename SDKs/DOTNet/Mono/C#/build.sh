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
	cp -R ./Resources ./bin/Debug
	if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi
fi