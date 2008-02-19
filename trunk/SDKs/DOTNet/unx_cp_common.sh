#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

cpTo() {
	BASE_DIR=./Mono/${1}
	BASE_DIR=`cd ${BASE_DIR}; pwd`
	if [ $? != 0 ]; then echo "Cannot find dir for $1 - ${BASE_DIR}"; exit 1; fi
			
	# Copy all files from the Base folder to Mono.
	find ../Base/All -maxdepth 1 -type f -exec cp {} ${BASE_DIR} \;

	if [ -d ${BASE_DIR}/Resources ]
	then
		echo "  ... Removing Old Resources Directory from $1"
		rm -rf ${BASE_DIR}/Resources
	fi

	echo "  ... Creating Resources Directory"
	mkdir ${BASE_DIR}/Resources
	mkdir ${BASE_DIR}/Resources/fonts
	mkdir ${BASE_DIR}/Resources/images
	mkdir ${BASE_DIR}/Resources/sounds
	mkdir ${BASE_DIR}/Resources/maps

	echo "  ... Copying Resources"

	SOURCE_RESOURCE=../Base/All/Resources
	RESOURCE_DIR=${BASE_DIR}/Resources

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
	find ../Base/DOTNet/${1} -maxdepth 1 -type f -exec cp {} ${BASE_DIR} \;
	if [ $? != 0 ]; then echo "Error copying code"; exit 1; fi

	if [ $1 = "C#" ]
	then
		if [ ! -d ${BASE_DIR}/Properties ]
		then
			mkdir ${BASE_DIR}/Properties
		fi
	
		cp ../Base/DOTNet/${1}/Properties/* ${BASE_DIR}/Properties
		if [ $? != 0 ]; then echo "Error copying Properties"; exit 1; fi
	else
		if [ ! -d ${BASE_DIR}/My\ Project ]
		then
			mkdir ${BASE_DIR}/My\ Project
		fi

		cp ../Base/DOTNet/${1}/My\ Project/* ${BASE_DIR}/My\ Project
		if [ $? != 0 ]; then echo "Error copying My Project"; exit 1; fi	
	fi

	echo "  Done - $1"
}

echo "__________________________________________________"
echo "Copying Mono Files"
echo "__________________________________________________"

cpTo "C#"
cpTo VB

echo "  Finished"
echo "__________________________________________________"