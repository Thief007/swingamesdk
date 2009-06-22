#!/bin/sh

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

#Set the basic paths
OUT_DIR="${APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"
LIB_DIR="${APP_PATH}/lib"
LOG_FILE="${APP_PATH}/out.log"

C_FLAGS="-O3 -Wall"
SG_INC="-I${APP_PATH}/lib/"

GCC_BIN=`which gcc`

GAME_NAME=${APP_PATH##*/}

CLEAN="N"

Usage()
{
    echo "Usage: [-c] [-h] [name]"
    echo 
    echo "Compiles your game into an executable application."
    echo "Output is located in $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -h   Show this help message "
    exit 0
}

while getopts chd o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    d)  C_FLAGS="-g -Wall"
    esac
done

shift $((${OPTIND}-1))

if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


CleanTmp()
{
    if [ -d "${TMP_DIR}" ]
    then
        rm -rf "${TMP_DIR}"
    fi
    mkdir "${TMP_DIR}"
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doMacCompile()
{
    CleanTmp
    
    echo "  ... Compiling for $1"
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]c$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.c} # %% = delete longest match from back, i.e. extract .c
        echo "      ... Compiling ${name}"    
        ${GCC_BIN} -c -arch ${1} ${SG_INC} ${C_FLAGS} -o "${TMP_DIR}/${name}.o" ${file} >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling"; cat ${LOG_FILE}; exit 1; fi
    done
    
    #Assemble all of the .s files
    echo "  ... Creating game for $1"
    FRAMEWORKS=`ls -d ${LIB_DIR}/*.framework | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`
    
    ${GCC_BIN} -F${LIB_DIR} ${FRAMEWORKS} -arch $1 -o ${OUT_DIR}/${GAME_NAME}.${1} `find ${TMP_DIR} -name \*.o -maxdepth 1`
    if [ $? != 0 ]; then echo "Error creating game"; cat ${LOG_FILE}; exit 1; fi
    
    CleanTmp
}

# 
# Create fat executable (i386 + ppc)
# 
doLipo()
{
    echo "  ... Creating Universal Binary"
    lipo -arch ${1} "${OUT_DIR}/${GAME_NAME}.${1}" -arch ${2} "${OUT_DIR}/${GAME_NAME}.${2}" -output "${OUT_DIR}/${GAME_NAME}" -create
    
    rm -f "${OUT_DIR}/${GAME_NAME}.${1}"
    rm -f "${OUT_DIR}/${GAME_NAME}.${2}"
}

doMacPackage()
{
    if [ -d ./bin/"${GAME_NAME}.app" ] 
    then
    	echo "  ... Removing old application"
    	rm -rf ./bin/"${GAME_NAME}.app"
    fi

    echo "  ... Creating Application Bundle"

    mkdir "./bin/${GAME_NAME}.app"
    mkdir "./bin/${GAME_NAME}.app/Contents"
    mkdir "./bin/${GAME_NAME}.app/Contents/MacOS"
    mkdir "./bin/${GAME_NAME}.app/Contents/Resources"
    mkdir "./bin/${GAME_NAME}.app/Contents/Frameworks"

    echo "  ... Added Private Frameworks"
    cp -R ./lib/*.framework "./bin/${GAME_NAME}.app/Contents/Frameworks/"

    mv bin/"${GAME_NAME}" "./bin/${GAME_NAME}.app/Contents/MacOS/" 

    echo "<?xml version='1.0' encoding='UTF-8'?>\
    <!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
    <plist version=\"1.0\">\
    <dict>\
            <key>CFBundleDevelopmentRegion</key>\
            <string>English</string>\
            <key>CFBundleExecutable</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundleIconFile</key>\
            <string>${ICON}</string>\
            <key>CFBundleIdentifier</key>\
            <string>au.edu.swinburne.${GAME_NAME}</string>\
            <key>CFBundleInfoDictionaryVersion</key>\
            <string>6.0</string>\
            <key>CFBundleName</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundlePackageType</key>\
            <string>APPL</string>\
            <key>CFBundleSignature</key>\
            <string>SWIN</string>\
            <key>CFBundleVersion</key>\
            <string>1.0</string>\
            <key>CSResourcesFileMapped</key>\
            <true/>\
    </dict>\
    </plist>" >> "./bin/${GAME_NAME}.app/Contents/Info.plist"

    echo "APPLSWIN" >> "./bin/${GAME_NAME}.app/Contents/PkgInfo"

    RESOURCE_DIR="./bin/${GAME_NAME}.app/Contents/Resources"
}

doLinuxCompile()
{
    CleanTmp
    
    for file in `find ${APP_PATH} -mindepth 2 | grep [.]c$`
    do
        name=${file##*/} # ## = delete longest match for */... ie all but file name
        name=${name%%.c} # %% = delete longest match from back, i.e. extract .c
        echo "      ... Compiling ${name}"    
        ${GCC_BIN} -c ${SG_INC} ${C_FLAGS} -o "${TMP_DIR}/${name}.o" ${file} >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error compiling SwinGame C wrapper"; cat ${LOG_FILE}; exit 1; fi
    done
    
    #Assemble all of the .s files
    echo "  ... Creating game"
    
    ${GCC_BIN} -L${LIB_DIR} -lsgsdk -o ${OUT_DIR}/${GAME_NAME} `find ${TMP_DIR} -name \*.o -maxdepth 1`
    if [ $? != 0 ]; then echo "Error creating game"; cat ${LOG_FILE}; exit 1; fi
    
    CleanTmp
}

doLinuxPackage()
{
    cp ./lib/libsgsdk.so "./bin/"
}


if [ $CLEAN = "N" ]
then
    if [ ! -d "${OUT_DIR}" ]
    then
        mkdir -p "${OUT_DIR}"
    fi
    
    if [ -f /System/Library/Frameworks/Cocoa.framework/Cocoa ]
    then
        echo "--------------------------------------------------"
        echo "          Creating $GAME_NAME"
        echo "                 for Mac OS X"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
        echo "--------------------------------------------------"
        echo "  ... Creating ${GAME_NAME}"
        
        doMacCompile "ppc"
        doMacCompile "i386"
        
        doLipo "i386" "ppc"
        doMacPackage
    else
        echo "--------------------------------------------------"
        echo "          Creating SwinGame C Library"
        echo "                 for Linux"
        echo "--------------------------------------------------"
        echo "  Running script from $APP_PATH"
        echo "  Saving output to $OUT_DIR"
        echo "--------------------------------------------------"
        echo "  ... Creating C wrapper library"
        CreateCWrapper >> ${LOG_FILE}
        if [ $? != 0 ]; then echo "Error creating c wrapper library"; cat ${LOG_FILE}; exit 1; fi
        
        echo "  ... Compiling Library"
        doLinuxCompile
        
        doLinuxPackage
    fi
else
    CleanTmp
    rm -rf "${OUT_DIR}"
    mkdir "${OUT_DIR}"
    echo    ... Cleaned
fi

#remove temp files on success
rm -f ${LOG_FILE} 2>> /dev/null
rm -rf ${TMP_DIR} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"