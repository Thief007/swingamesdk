#!/bin/bash

#clean
rm -rf tmp
mkdir tmp

# declare variables

if [ ${1} = "-iphone" ]; then
  iphone=true
elif [ ${1} = "-ipad"]; then
  ipad=true
fi

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH
GAME_NAME=${APP_PATH##*/}

APP_PATH="."
LOG_FILE="${APP_PATH}/out.log"
DRV_LIB=`find ./libsrc -type d ! -path \*.svn\* | awk -F . '{print "-Fu"$0}'`
SG_INC="-Fi${APP_PATH}/libsrc -Fu${APP_PATH}/libsrc -Fu${APP_PATH}/src ${DRV_LIB}"
homeDir=`echo ~`



DoExitCompile ()
{ 
    echo "An error occurred while compiling"; 
    cat out.log
    exit 1;  
}
echo "--------------------------------------------------"
echo "          Creating $GAME_NAME"
echo "          for iOS"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
echo "  Saving output to $OUT_DIR"
echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
echo "--------------------------------------------------"
echo "  ... Compiling ${GAME_NAME}"

ppc386 -gw -Mobjfpc -Sew -Sh ${SG_INC} -XX -k-ios_version_min -k5.0 -XR"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk" -gltw -FE"tmp" -FU"tmp" -Fi"src" -Fu"lib/mac/static_libraries" -k"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk/usr/lib/libbz2.dylib" -k"lib/mac/static_libraries/libSDL.a" -k"lib/mac/static_libraries/libpng.a" -k"lib/mac/static_libraries/libSDL_gfx.a" -k"lib/mac/static_libraries/libSDL_mixer.a" -k"lib/mac/static_libraries/libSDL_image.a" -k"lib/mac/static_libraries/libSDL_ttf.a" -k"lib/mac/static_libraries/libfreetype.a" -k"lib/mac/static_libraries/libogg.a" -k"lib/mac/static_libraries/libvorbis.a"  -Fu/Developer/ObjectivePascal/units/i386-iphonesim -Tiphonesim  -o"./${GAME_NAME}" test/CircleCollide.pas -k-framework -kAudioToolbox -k-framework -kQuartzCore -k-framework -kOpenGLES -k-framework -kCoreGraphics -k"-framework MobileCoreServices" -k"-framework ImageIO" -k-framework -kUIKit -k-framework -kFoundation -k-framework -kCoreAudio -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_SDL13 > ${LOG_FILE} 2> ${LOG_FILE}

if [ $? != 0 ]; then
   DoExitCompile;
fi

#Generates UUID and save to txt file for future use.
FILE=UUID.txt
UUID=""
if [ -f $FILE ];
then
  echo "  ... UUID file found."
  echo "  ... Reading UUID file."
  while read line    
  do    
    UUID=$line    
  done <UUID.txt
else
  echo "  ... UUID file not found."
  echo "  ... Generating UUID."
  $UUID=`echo uuidgen`
  echo "  ... Saving UUID file."
  echo $UUID>UUID.txt   
fi


OUT_DIR="${homeDir}/Library/Application Support/iPhone Simulator/5.0/Applications/${UUID}/"

OUT_APP_DIR="${OUT_DIR}${GAME_NAME}.app/"


echo "  ... Installing ${GAME_NAME} to iOS Simulator."
#move file to simulator install folder

if ! [ -d "${OUT_APP_DIR}" ]; then
  mkdir -p "${OUT_APP_DIR}"
fi


mv "${GAME_NAME}" "${OUT_APP_DIR}${GAME_NAME}"

#generate info.plist and pkginfo
if [ ${iphone} = true ]; then

  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>1</integer>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"

elif [${ipad} = true]; then
  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"
#else generate for both.
else
  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>1</integer>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"
fi
echo "APPLSWIN" > "${OUT_APP_DIR}/PkgInfo"

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -exec mkdir -p "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}


echo "  ... Copying resources."
mkdir -p "${OUT_DIR}Resources"
copyWithoutSVN "./test/Resources" "${OUT_DIR}/Resources"
rm -f ${LOG_FILE} 2>> /dev/null
echo "  ... Starting iOS Simulator."
#restart simulator
osascript <<EOF
tell application "System Events" 
  set SimulatorOpen to ("iPhone Simulator") 
end tell
tell application "iPhone Simulator"
  if (SimulatorOpen contains "iPhone Simulator") is true then 
      quit
      delay 1
    end if
    activate
end tell
EOF

echo "--------------------------------------------------"
echo "Finished!"
