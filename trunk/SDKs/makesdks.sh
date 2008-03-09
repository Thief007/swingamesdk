#!/bin/sh

VERSION="1.1.2"
VERSION_TXT="1-1-2"

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	FILE_OS="Mac"
else
	FILE_OS="Unx"
fi

echo "__________________________________________________"
echo " Creating $FILE_OS SDKs for - $VERSION"
echo "__________________________________________________"


# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#
# Pack folder $1 to distribute
# With name $2
#
pack() {
	BASE_SDK_DIR="${APP_PATH}/SGSDK-${VERSION_TXT}-${2}-${FILE_OS}"
	SDK_DIR=${BASE_SDK_DIR}/SwinGame
		
	if [ -f "${BASE_SDK_DIR}.dmg" ]
	then
		echo "  Removing old SDK - ${BASE_SDK_DIR}.dmg"
		rm "${BASE_SDK_DIR}.dmg"
	fi
	
	if [ -d "${BASE_SDK_DIR}" ]
	then
		rm -rf "${BASE_SDK_DIR}"		
	fi
	
	mkdir -p "$SDK_DIR"
	pushd . >> /dev/null

	cd "$1"

	echo "  Copying SDK to temporary location"	
	find . -type d ! -path *.svn* ! -path */bin/* -mindepth 1 -exec mkdir "${SDK_DIR}/{}" \;
	find . -type f ! -path *.svn* ! -path */bin/* -exec cp "{}" "${SDK_DIR}/{}" \;

	if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
	then
		echo "  Creating disk image"
		hdiutil create -srcfolder "${BASE_SDK_DIR}" "${BASE_SDK_DIR}.dmg" >> /dev/null
	else
		echo "  Combining files and zipping"
		tar -cvf "${BASE_SDK_DIR}.tar"
		gzip "${BASE_SDK_DIR}.tar"
	fi

	echo "  Removing temporary folder"
	rm -rf "${BASE_SDK_DIR}"		
	
	popd >> /dev/null
	echo "  Done ${BASE_SDK_DIR}"
}


pack ${APP_PATH}/DOTNet/Mono/C# "C#"
pack ${APP_PATH}/DOTNet/Mono/VB "VB.NET"
pack ${APP_PATH}/Pascal/Unx/FPC "FPC"