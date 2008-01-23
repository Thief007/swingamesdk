#!/bin/sh

echo "__________________________________________________"
echo "Package Mac Application"
echo "__________________________________________________"

Usage()
{
	echo "Usage: $0 [-e executable] [-i icon] [-h] [-r] productName"
	echo 
	echo "Options:"
	echo " -h = Show usage" 
	echo " -r = Use Release build"
	echo " -i icon = Use the specified icon, defaults to SwinGame"
	echo " -e executable = Use this as the name of the exe, searched by default"
	exit 1
}

ICON=SwinGame
EXTRA_OPTS=""
BIN_DIR=bin/Debug

while getopts i:e:hr o
do
	case "$o" in
	i)	 ICON="$OPTARG";;
	e)	 EXECUTABLE_NAME="$OPTARG";;
	d)  EXTRA_OPTS="-gw -gl -gc -gp -Ci -Co -Ct";;
	h)  Usage;;
	r)  BIN_DIR=bin/Release
	esac
done

shift $((${OPTIND}-1))

if [ -n "$1" ] 
then
	PRODUCT_NAME="$1"
else
	Usage
fi

echo "  Creating Application Bundle"

if [ -d "$BIN_DIR/${PRODUCT_NAME}.app" ] 
then
	echo "  ... Removing Old Application"
	rm -rf "$BIN_DIR/${PRODUCT_NAME}.app"
fi

cd "$BIN_DIR"

if [ -z ${EXECUTABLE_NAME} ]
then
	echo "  ... Finding Executable"
	EXECUTABLE_NAME=`ls *.exe`
fi

echo "  ... Packaging ${EXECUTABLE_NAME}"

macpack -m winforms -n ${PRODUCT_NAME} -r SGSDK.NET.dll -r libSGSDK.dylib ${EXECUTABLE_NAME}

echo "  ... Adding Resources"
cp -R ../Resources/ "${PRODUCT_NAME}.app/Contents/Resources"

echo "  ... Setting Application Information"

rm -f "${PRODUCT_NAME}.app/Contents/Info.plist"

echo "<?xml version='1.0' encoding='UTF-8'?>\
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
<plist version=\"1.0\">\
<dict>\
        <key>CFBundleDevelopmentRegion</key>\
        <string>English</string>\
        <key>CFBundleExecutable</key>\
        <string>${PRODUCT_NAME}</string>\
        <key>CFBundleIconFile</key>\
        <string>${ICON}</string>\
        <key>CFBundleIdentifier</key>\
        <string>au.edu.swinburne.${PRODUCT_NAME}</string>\
        <key>CFBundleInfoDictionaryVersion</key>\
        <string>6.0</string>\
        <key>CFBundleName</key>\
        <string>${PRODUCT_NAME}</string>\
        <key>CFBundlePackageType</key>\
        <string>APPL</string>\
        <key>CFBundleSignature</key>\
        <string>SWIN</string>\
        <key>CFBundleVersion</key>\
        <string>1.0</string>\
        <key>CSResourcesFileMapped</key>\
        <true/>\
</dict>\
</plist>" >> "${PRODUCT_NAME}.app/Contents/Info.plist"

echo "  ... Adding Package Information"
echo "APPLSWIN" >> "${PRODUCT_NAME}.app/Contents/PkgInfo"

echo "  ... Cleaning up"

rm -rf ../Resources
rm -f SGSDK.NET.dll
rm -f libSGSDK.dylib
rm -f ${EXECUTABLE_NAME}
rm -f *.XML
rm -f *.mdb

echo "  Created ${BIN_DIR}/${PRODUCT_NAME}.app"
echo "  Finished"
echo "__________________________________________________"
