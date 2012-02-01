#!/bin/bash

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH
APP_PATH="."

DRV_LIB=`find ./libsrc -type d ! -path \*.svn\* | awk -F . '{print "-Fu"$0}'`
SG_INC="-Fi${APP_PATH}/libsrc -Fu${APP_PATH}/libsrc -Fu${APP_PATH}/src ${DRV_LIB}"

ppc386 -gw -Mobjfpc -Sew -Sh ${SG_INC} -XX -k-ios_version_min -k5.0 -XR"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk" -gltw -FE"tmp" -FU"tmp" -Fi"src" -Fu"lib/mac/static_libraries" -k"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk/usr/lib/libbz2.dylib" -k"lib/mac/static_libraries/libSDL.a" -k"lib/mac/static_libraries/libpng.a" -k"lib/mac/static_libraries/libSDL_gfx.a" -k"lib/mac/static_libraries/libSDL_mixer.a" -k"lib/mac/static_libraries/libSDL_image.a" -k"lib/mac/static_libraries/libSDL_ttf.a" -k"lib/mac/static_libraries/libfreetype.a" -k"lib/mac/static_libraries/libogg.a" -k"lib/mac/static_libraries/libvorbis.a"  -Fu/Developer/ObjectivePascal/units/i386-iphonesim -Tiphonesim  -o./foo test/CircleCollide.pas -k-framework -kAudioToolbox -k-framework -kQuartzCore -k-framework -kOpenGLES -k-framework -kCoreGraphics -k"-framework MobileCoreServices" -k"-framework ImageIO" -k-framework -kUIKit -k-framework -kFoundation -k-framework -kCoreAudio -k-no_order_inits -XMSDL_main -dIOS


#Generates UUID and save to txt file for future use.
FILE=UUID.txt
UUID=""
if [ -f $FILE ];
then
   while read line    
   do    
       UUID=$line    
   done <UUID.txt
else
   $UUID=`echo uuidgen`
   
   echo $UUID>UUID.txt   
fi



#move file to simulator install folder
mv foo "/Users/admin/Library/Application Support/iPhone Simulator/5.0/Applications/"$UUID"/Touch.app/Touch"


#restart simulator
osascript <<EOF 
tell application "iPhone Simulator"
	quit
	delay 1
	activate
end tell
EOF


