#!/bin/bash


# declare variables

# if [ "${1}" = "-iphone" ]; then
#   iphone=true
# elif [ "${1}" = "-ipad" ]; then
#   ipad=true
# fi

# Get path to script - ensure we start here
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

FULL_APP_PATH=$APP_PATH
APP_PATH="."

GAME_NAME=${FULL_APP_PATH##*/}

OUT_DIR="${APP_PATH}/bin"
LIB_DIR="${APP_PATH}/lib/godly/mac"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"

LOG_FILE="${APP_PATH}/out.log"

SG_INC="-Fi${APP_PATH}/lib -Fu${APP_PATH}/lib -Fu${APP_PATH}/src"


# process options
while getopts n: o
do
    case "$o" in
    n)  GAME_NAME="${OPTARG}";;
    esac
done



locateGameMain()
{
  cd "${SRC_DIR}"
  fileList=$(find "." -maxdepth 1 -type f -name \*.pas)
  FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)
  
  if [ ${FILE_COUNT} = 1 ]; then
    GAME_MAIN=${fileList[0]}
  else
    echo "Select the file to compile for your game"
    PS3="File number: "
  
    select fileName in $fileList; do
        if [ -n "$fileName" ]; then
            GAME_MAIN=${fileName}
        fi
      
        break
    done
  fi
  
  cd ${FULL_APP_PATH}
  
  if [ ! -f "${SRC_DIR}/${GAME_MAIN}" ]; then
    echo "Cannot find file to compile, was looking for ${GAME_MAIN}"
    exit -1
  fi
}

DoExitCompile ()
{ 
    echo "An error occurred while compiling"; 
    cat out.log
    exit 1;  
}

locateGameMain

echo "--------------------------------------------------"
echo "          Creating $GAME_NAME"
echo "          for iOS"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
#echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
echo "--------------------------------------------------"
echo "  ... Compiling ${GAME_NAME}"

if [ ! -d ${TMP_DIR} ]; then
    mkdir -p "${TMP_DIR}"
    mkdir "${TMP_DIR}/arm" "${TMP_DIR}/i386"
fi

if [ ! -d ${OUT_DIR} ]; then
    mkdir "${OUT_DIR}"
fi


ppcarm -gw -S2 -Sew -Cparmv7 -Cfvfpv2 -Sh ${SG_INC} -XX -k-ios_version_min -k5.0 -XR"/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk" -gltw -FE"tmp/arm" -FU"tmp/arm" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -k"-framework AudioToolbox -framework QuartzCore -framework OpenGLES -framework CoreGraphics" -k"-framework MobileCoreServices" -k"-framework ImageIO" -k"-framework UIKit -framework Foundation -framework CoreAudio" -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 -o"${TMP_DIR}/${GAME_NAME}.arm" "src/${GAME_MAIN}" > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
   DoExitCompile; 
fi

ppc386 -gw -S2 -Sew -Sh ${SG_INC} -XX -k-ios_version_min -k5.0 -XR"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk" -gltw -FE"tmp/i386" -FU"tmp/i386" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -o"${TMP_DIR}/${GAME_NAME}.i386" "src/${GAME_MAIN}" -k-framework -kAudioToolbox -k-framework -kQuartzCore -k-framework -kOpenGLES -k-framework -kCoreGraphics -k"-framework MobileCoreServices" -k"-framework ImageIO" -k-framework -kUIKit -k-framework -kFoundation -k-framework -kCoreAudio -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
   DoExitCompile;
fi

lipo -create -output "${OUT_DIR}/${GAME_NAME}" "${TMP_DIR}/${GAME_NAME}.i386" "${TMP_DIR}/${GAME_NAME}.arm" > ${LOG_FILE} 2> ${LOG_FILE}

/Developer/usr/bin/dsymutil --verbose "${OUT_DIR}/${GAME_NAME}" -o "${OUT_DIR}/${GAME_NAME}.app.dSYM" > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
    echo 'Failed to create debug symbols'
    cat out.log
    exit 1;
fi

echo "  ... Creating XCode Project"
python ${APP_PATH}/tools/create_xcode_project.py ${GAME_NAME} > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
    echo 'Failed to create xcode project'
    cat out.log
    exit 1;
fi

open ${GAME_NAME}.xcodeproj

echo "--------------------------------------------------"
echo "Finished!"
