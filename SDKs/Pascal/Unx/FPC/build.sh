#!/bin/sh

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

ICON=SwinGame
EXTRA_OPTS="-O3"
DEBUG="N"

while getopts i:e:hd o
do
	case "$o" in
	i)	 ICON="$OPTARG";;
	e)	 EXECUTABLE_NAME="$OPTARG";;
	d)   DEBUG="Y";;
	h)   Usage;;
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
	if [ DEBUG = "Y" ]
	then
		EXTRA_OPTS="-gw3 -gl -gp -godwarfsets -Ci -Co -Ct"
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
	fi

	echo "  ... Compiling Game"
	
	fpc $EXTRA_OPTS -XMSDL_main -Mdelphi -FE./bin -Fu./lib -s GameLauncher.pas > out.log
	if [ $? != 0 ]; then DoExitCompile; fi
	rm -f out.log

	#Remove the pascal assembler script
	rm bin/ppas.sh

	echo "  ... Assembling $PRODUCT_NAME from:"

	#Assemble all of the .s files
	for file in `find . | grep [.]s$` #`ls *.s`
	do
		echo "  ... - $file"
		/usr/bin/as -o ${file%.s}.o $file -arch i386
		if [ $? != 0 ]; then DoExitAsm $file; fi
		rm $file
	done

	echo "  ... Linking ${EXECUTABLE_NAME}"
	#Removed -s option - if this crashes... maybe re-add it
	/usr/bin/ld  -L./lib -L/usr/X11R6/lib -L/usr/lib -search_paths_first -multiply_defined suppress -L. -o "${EXECUTABLE_NAME}" `cat bin/link.res` -lSDL -lSDL_mixer -lsmpeg -lSDL_image -lSDL_TTF -lSDL_gfx -lz -lstdc++ -ltiff -lpng12 -lSDLmain -logg -lfreetype -ljpeg -lvorbis -lvorbisenc -lvorbisfile -framework Cocoa -framework QuickTime -framework CoreFoundation -framework IOKit -framework AudioUnit -framework Carbon -framework OpenGL -lgcc -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib
	if [ $? != 0 ]; then DoExitLink ${EXECUTABLE_NAME}; fi

	rm bin/link.res
	rm bin/*.o
	rm bin/*.ppu

	if [ -d ./bin/"${PRODUCT_NAME}.app" ] 
	then
		echo "  ... Removing old application"
		rm -rf ./bin/"${PRODUCT_NAME}.app"
	fi

	echo "  ... Creating Application Bundle"

	mkdir ./bin/"${PRODUCT_NAME}.app"
	mkdir ./bin/"${PRODUCT_NAME}.app/Contents"
	mkdir ./bin/"${PRODUCT_NAME}.app/Contents/MacOS"
	mkdir ./bin/"${PRODUCT_NAME}.app/Contents/Resources"

	mv "${EXECUTABLE_NAME}" "./bin/${PRODUCT_NAME}.app/Contents/MacOS/" 

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
	if [ DEBUG = "Y" ]
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

	fpc -XMSDL_main -Mdelphi -FE./bin -Fu./lib -FU./bin -o"$PRODUCT_NAME" $EXTRA_OPTS GameLauncher.pas 

	rm ./bin/*.o ./bin/*.ppu

	RESOURCE_DIR="./bin/Resources"
fi

SOURCE_RESOURCE="./Resources"

if [ -d ${RESOURCE_DIR} ]
then
	echo "  ... Removing Old Resources"
	rm -rf ${RESOURCE_DIR}
fi

echo "  ... Copying Resources"

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
echo "__________________________________________________"

echo   Finished
	