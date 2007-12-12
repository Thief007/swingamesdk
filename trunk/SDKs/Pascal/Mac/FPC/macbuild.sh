#!/bin/sh

Usage()
{
	echo "Usage: $0 [-e executable] [-i icon] [-d] [-h] productName"
	echo " -d = Debug       -h = Show usage"
	exit 1
}

ICON=SwinGame
EXTRA_OPTS=""

while getopts i:s:hd o
do
	case "$o" in
	i)	 ICON="$OPTARG";;
	s)	 EXECUTABLE_NAME="$OPTARG";;
	d)   EXTRA_OPTS="-gw -gl -gc -gp -Ci -Co -Ct";;
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

echo "Building $EXECUTABLE_NAME with icon $ICON.icns as $PRODUCT_NAME"

DoExitCompile ()
{ 
	echo "An error occurred while compiling"; 
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

echo Compiling game

mkdir -p ./bin

fpc $EXTRA_OPTS -XMSDL_main -Mdelphi -FE./bin -Fu./lib -s GameLauncher.pas 
if [ $? != 0 ]; then DoExitCompile; fi

#Remove the pascal assembler script
rm bin/ppas.sh

echo Assembling mygame

#Assemble all of the .s files
for file in `find . | grep [.]s$` #`ls *.s`
do
	echo ... $file
	/usr/bin/as -o ${file%.s}.o $file -arch i386
	if [ $? != 0 ]; then DoExitAsm $file; fi
	rm $file
done

echo Linking ${EXECUTABLE_NAME}
/usr/bin/ld  -L./lib -L/usr/X11R6/lib -L/usr/lib -search_paths_first -multiply_defined suppress -L. -o "${EXECUTABLE_NAME}" `cat bin/link.res` -lSDL -lSDL_mixer -lsmpeg -lSDL_image -lSDL_TTF -lSDL_gfx -lz -lstdc++ -ltiff -lpng12 -lSDLmain -logg -lfreetype -ljpeg -lvorbis -lvorbisenc -lvorbisfile -framework Cocoa -framework QuickTime -framework CoreFoundation -framework IOKit -framework AudioUnit -framework Carbon -framework OpenGL -lgcc -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib

if [ $? != 0 ]; then DoExitLink MyGame; fi
cd bin/
rm link.res
rm *.o
rm *.ppu
cd ../

echo Creating Bundle

if [ -d ./bin/"${PRODUCT_NAME}.app" ] 
then
	echo Removing old application
	rm -rf ./bin/"${PRODUCT_NAME}.app"
fi
mkdir ./bin/"${PRODUCT_NAME}.app"
mkdir ./bin/"${PRODUCT_NAME}.app/Contents"
mkdir ./bin/"${PRODUCT_NAME}.app/Contents/MacOS"
mkdir ./bin/"${PRODUCT_NAME}.app/Contents/Resources"

mv "${EXECUTABLE_NAME}" "./bin/${PRODUCT_NAME}.app/Contents/MacOS/" 
cp -r ./Resources "./bin/${PRODUCT_NAME}.app/Contents"
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