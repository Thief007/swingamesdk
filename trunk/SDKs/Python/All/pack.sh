#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

PYTHON_PATH="/System/Library/Frameworks/Python.framework/Versions/2.5/Resources/Python.app/Contents/MacOS/Python"

echo "__________________________________________________"
echo "Package Mac Application"
echo "__________________________________________________"

Usage()
{
	echo "Usage: $0 [-i icon] [-e script.py] [-h] [-r] productName"
	echo 
	echo "Options:"
	echo " -h = Show usage" 
	echo " -e = Set the executed python script "
	echo " -r = Use Release build"
	echo " -i icon = Use the specified icon, defaults to SwinGame"
	exit 1
}

ICON=SwinGame
EXTRA_OPTS=""
BIN_DIR=.

while getopts i:e:hr o
do
	case "$o" in
	i)	 ICON="$OPTARG";;
	e)	 PY_NAME="$OPTARG";;
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

if [ -z ${PY_NAME} ]
then
	PY_NAME="GameLauncher.py"
fi

if [ ! -f ${PY_NAME} ]
then
	Usage
fi

echo "  Creating Application Bundle"

if [ -d "$BIN_DIR/${PRODUCT_NAME}.app" ] 
then
	echo "  ... Removing Old Application"
	rm -rf "$BIN_DIR/${PRODUCT_NAME}.app"
fi

cd "$BIN_DIR"

echo "  ... Packaging ${PY_NAME}"

#macpack -m winforms -n "${PRODUCT_NAME}" -r SGSDK.NET.dll -r libSGSDK.dylib "${EXECUTABLE_NAME}"
mkdir "${PRODUCT_NAME}.app"
mkdir "${PRODUCT_NAME}.app/Contents"
mkdir "${PRODUCT_NAME}.app/Contents/MacOS"
mkdir "${PRODUCT_NAME}.app/Contents/Resources"
mkdir "${PRODUCT_NAME}.app/Contents/Resources/sgsdk"
mkdir "${PRODUCT_NAME}.app/Contents/Frameworks"

# cp SGSDK.NET.dll "${PRODUCT_NAME}.app/Contents/MacOS"
cp ./sgsdk/libSGSDK.dylib "${PRODUCT_NAME}.app/Contents/Resources/sgsdk"
cp *.py "${PRODUCT_NAME}.app/Contents/Resources"
cp ./sgsdk/*.py "${PRODUCT_NAME}.app/Contents/Resources/sgsdk"

echo "  ... Adding Resources"
cp -R ./Resources/ "${PRODUCT_NAME}.app/Contents/Resources"

echo "  ... Added Private Frameworks"
cp -R ./sgsdk/Frameworks/* "${PRODUCT_NAME}.app/Contents/Frameworks/"

pushd . >> /dev/null

cd "${PRODUCT_NAME}.app/Contents/Resources"
ln -s ../Frameworks Frameworks
popd >> /dev/null

echo "#!/bin/sh
PWD=\`pwd\`
# Fetch the path relative to the launch point where this shell script exists.
APP_PATH=\`echo \$0 | awk '{split(\$0,patharr,\"/\"); idx=1; while(patharr[idx+3] != \"\") { if (patharr[idx] != \"/\") {printf(\"%s/\", patharr[idx]); idx++ }} }'\`

if [ -f \$APP_PATH/Contents/Resources/MyPy ]
then
	rm -f \$APP_PATH/Contents/Resources/MyPy
fi

ln -s ${PYTHON_PATH} \$APP_PATH/Contents/Resources/MyPy 

cd \$APP_PATH/Contents/Resources
./MyPy ${PY_NAME}
" >> "${PRODUCT_NAME}.app/Contents/MacOS/${PRODUCT_NAME}"

chmod a+x "${PRODUCT_NAME}.app/Contents/MacOS/${PRODUCT_NAME}"

#
# Creating App info
#

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
rm -rf ../Frameworks
rm -f SGSDK.NET.dll
rm -f libSGSDK.dylib
rm -f "${EXECUTABLE_NAME}"
rm -f *.XML
rm -f *.mdb

echo "  Created ${BIN_DIR}/${PRODUCT_NAME}.app"
echo "  Finished"
echo "__________________________________________________"
