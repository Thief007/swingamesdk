#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd $APP_PATH

DoExitCompile ()
{ 
	echo "An error occurred while compiling"; 
	cat out.log
	exit 1; 
}
DoExitAsm ()
{ 
	echo "An error occurred while assembling $1"; 
	exit 1; 
}
DoExitLink ()
{ 
	echo "An error occurred while linking $1"; 
	exit 1; 
}
Usage()
{
	echo "Usage: $0 [-e executable] [-i icon] [-d] [-h] productName"
	echo
	echo "Compile the Pascal SwinGame with name 'productName'"
	echo
	echo "Options:"
	echo "  -d = Debug"
	echo "  -h = Show usage"
	echo
	echo "Mac Only Options:"
	echo "  -e = Set the name of the executable, defaults to productName"
	echo "  -i = Set the name of the icon file eg: Test for Test.icns"
	exit 1
}

# Parameters: 
# 1: arch
DoMacCompile()
{
	echo "  ... Compiling Game - $1"
	if [ ! -d bin/${1} ] 
	then
		mkdir bin/${1}
	else
		rm bin/${1}/link.res 2>> /dev/null
		rm bin/${1}/*.o 2>> /dev/null
		rm bin/${1}/*.ppu 2>> /dev/null
		rm bin/${1}/*.s 2>> /dev/null
	fi

	${FPC_BIN}  $EXTRA_OPTS -Mdelphi -FE./bin/${1} -Fi./bin/${1} -Fu./lib/${1} -s GameLauncher.pas > out.log
	if [ $? != 0 ]; then DoExitCompile; fi
	rm -f out.log

	#Remove the pascal assembler script
	rm bin/${1}/ppas.sh

	echo "  ... Assembling for $1"

	#Assemble all of the .s files
	for file in `find . | grep [.]s$` #`ls *.s`
	do
		/usr/bin/as -o ${file%.s}.o $file -arch ${1}
		if [ $? != 0 ]; then DoExitAsm $file; fi
		rm $file
	done

	echo "  ... Linking ${EXECUTABLE_NAME}"
	#Removed -s option - if this crashes... maybe re-add it
	/usr/bin/ld  -L./lib -L/usr/X11R6/lib -L/usr/lib -search_paths_first -multiply_defined suppress -L. -o "bin/${1}/${EXECUTABLE_NAME}" `cat bin/${1}/link.res` -framework Cocoa -F./lib -framework SDL -framework SDL_image -framework SDL_mixer -framework SDL_ttf
	if [ $? != 0 ]; then DoExitLink ${EXECUTABLE_NAME}; fi
	
	rm bin/${1}/link.res
	rm bin/${1}/*.ppu 2>> /dev/null
		
	if [ $DEBUG = "N" ] 
	then
		rm bin/${1}/*.o
	fi	
}

# Combine the two architectures
DoLipo()
{
	lipo -arch ${1} "bin/${1}/${EXECUTABLE_NAME}" -arch ${2} "bin/${2}/${EXECUTABLE_NAME}" -output "bin/${EXECUTABLE_NAME}" -create
	
	rm -rf "bin/${1}"
	rm -rf "bin/${2}"
}

ICON=SwinGame
EXTRA_OPTS="-O3"
DEBUG="N"
FPC_BIN=`which fpc`

while getopts i:e:hd o
do
	case "$o" in
	i)	ICON="$OPTARG";;
	e)	EXECUTABLE_NAME="$OPTARG";;
	d)  DEBUG="Y";;
	h)  Usage;;
	esac
done

shift $((${OPTIND}-1))

if [ -n "$1" ] 
then
	PRODUCT_NAME="$1"
else
	Usage
fi

if [ -z $EXECUTABLE_NAME ]
then
	EXECUTABLE_NAME="$PRODUCT_NAME"
fi

if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
then
	if [ $DEBUG = "Y" ]
	then
		EXTRA_OPTS="-gw -gl -gp -Ci -Co -Ct"
	fi

	echo "__________________________________________________"
	echo "Building Mac version"
	echo "__________________________________________________"	
	echo "  Product Name:     $PRODUCT_NAME"
	echo "  Executable Name:  $EXECUTABLE_NAME"
	echo "  Icon:             $ICON.icns"
	echo "  Compiler Options: $EXTRA_OPTS"
	echo "__________________________________________________"	

	if [ ! -d ./bin ]
	then
		echo "  ... Creating bin folder"
		mkdir ./bin
	else
		rm bin/link.res 2>> /dev/null
		rm bin/*.o 2>> /dev/null
		rm bin/*.ppu 2>> /dev/null
	fi

	FPC_BIN=`which ppc386`
	DoMacCompile "i386"
	FPC_BIN=`which ppcppc`
	DoMacCompile "ppc"

	DoLipo "i386" "ppc"

	if [ -d ./bin/"${PRODUCT_NAME}.app" ] 
	then
		echo "  ... Removing old application"
		rm -rf ./bin/"${PRODUCT_NAME}.app"
	fi

	echo "  ... Creating Application Bundle"

	mkdir "./bin/${PRODUCT_NAME}.app"
	mkdir "./bin/${PRODUCT_NAME}.app/Contents"
	mkdir "./bin/${PRODUCT_NAME}.app/Contents/MacOS"
	mkdir "./bin/${PRODUCT_NAME}.app/Contents/Resources"
	mkdir "./bin/${PRODUCT_NAME}.app/Contents/Frameworks"

	echo "  ... Added Private Frameworks"
	cp -R ./lib/*.framework "./bin/${PRODUCT_NAME}.app/Contents/Frameworks/"
	
	mv bin/"${EXECUTABLE_NAME}" "./bin/${PRODUCT_NAME}.app/Contents/MacOS/" 
	
	echo "<?xml version='1.0' encoding='UTF-8'?>\
	<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
	<plist version=\"1.0\">\
	<dict>\
	        <key>CFBundleDevelopmentRegion</key>\
	        <string>English</string>\
	        <key>CFBundleExecutable</key>\
	        <string>${EXECUTABLE_NAME}</string>\
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
	</plist>" >> "./bin/${PRODUCT_NAME}.app/Contents/Info.plist"

	echo "APPLSWIN" >> "./bin/${PRODUCT_NAME}.app/Contents/PkgInfo"
	
	RESOURCE_DIR="./bin/${PRODUCT_NAME}.app/Contents/Resources"
else
	if [ $DEBUG = "Y" ]
	then
		EXTRA_OPTS="-g -Ci -Co -Ct"
	fi

	echo "__________________________________________________"
	echo "Building Linux version"
	echo "__________________________________________________"	
	echo "  Product Name:     $PRODUCT_NAME"
	echo "  Executable Name:  $EXECUTABLE_NAME"
	echo "  Compiler Options: $EXTRA_OPTS"
	echo "__________________________________________________"	
	echo "  ... Compiling game"

	if [ ! -d ./bin ]
	then
		echo "  ... Creating bin folder"
		mkdir ./bin
	else
		rm ./bin/*.o ./bin/*.ppu 2> /dev/null
	fi

	${FPC_BIN}  -XMSDL_main -Mdelphi -FE./bin -Fu./lib -FU./bin -o"$PRODUCT_NAME" $EXTRA_OPTS GameLauncher.pas >> out.log
	if [ $? != 0 ]; then DoExitCompile; fi

	rm ./bin/*.o ./bin/*.ppu

	RESOURCE_DIR="./bin/Resources"
fi

SOURCE_RESOURCE="./Resources"

if [ -d "${RESOURCE_DIR}" ]
then
	echo "  ... Removing Old Resources"
	rm -rf "${RESOURCE_DIR}"
fi

echo "  ... Copying Resources"

mkdir "${RESOURCE_DIR}"
if [ $? != 0 ]; then echo "Error creating resource directory"; exit 1; fi
	
mkdir "${RESOURCE_DIR}/fonts"
if [ $? != 0 ]; then echo "Error creating fonts directory"; exit 1; fi
	
mkdir "${RESOURCE_DIR}/images"
if [ $? != 0 ]; then echo "Error creating images directory"; exit 1; fi
	
mkdir "${RESOURCE_DIR}/sounds"
if [ $? != 0 ]; then echo "Error creating sounds directory"; exit 1; fi
	
mkdir "${RESOURCE_DIR}/maps"
if [ $? != 0 ]; then echo "Error creating maps directory"; exit 1; fi

#cp ${SOURCE_RESOURCE}/* ${RESOURCE_DIR}
find ${SOURCE_RESOURCE} -maxdepth 1 -type f -exec cp {} "${RESOURCE_DIR}" \;
if [ $? != 0 ]; then echo "Error copying resources"; exit 1; fi
	 
#cp ${SOURCE_RESOURCE}/fonts/* ${RESOURCE_DIR}/fonts
find ${SOURCE_RESOURCE}/fonts -maxdepth 1 -type f -exec cp {} "${RESOURCE_DIR}/fonts" \;
if [ $? != 0 ]; then echo "Error copying fonts"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/images/* ${RESOURCE_DIR}/images
find ${SOURCE_RESOURCE}/images -maxdepth 1 -type f -exec cp {} "${RESOURCE_DIR}/images" \;
if [ $? != 0 ]; then echo "Error copying images"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/sounds/* ${RESOURCE_DIR}/sounds
find ${SOURCE_RESOURCE}/sounds -maxdepth 1 -type f -exec cp {} "${RESOURCE_DIR}/sounds" \;
if [ $? != 0 ]; then echo "Error copying sounds"; exit 1; fi
	
#cp ${SOURCE_RESOURCE}/maps/* ${RESOURCE_DIR}/maps	
find ${SOURCE_RESOURCE}/maps -maxdepth 1 -type f -exec cp {} "${RESOURCE_DIR}/maps" \;
if [ $? != 0 ]; then echo "Error copying maps"; exit 1; fi

echo "  Finished"
echo "__________________________________________________"
	